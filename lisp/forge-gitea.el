;;; forge-gitea.el --- Gitea support              -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

;;; Commentary
;;
;; See https://docs.gitea.io/en-us/api-usage/
;;     https://try.gitea.io/api/swagger

;;; Code:

(require 'gtea)
(require 'forge)

;;; Class

(defclass forge-gitea-repository (forge-repository)
  ((issues-url-format         :initform "https://%h/%o/%n/issues")
   (issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   ;; The anchor for the issue itself is .../%i#issue-%i
   (issue-post-url-format     :initform "https://%h/%o/%n/issues/%i#issuecomment-%I")
   (pullreqs-url-format       :initform "https://%h/%o/%n/pulls")
   (pullreq-url-format        :initform "https://%h/%o/%n/pulls/%i")
   (pullreq-post-url-format   :initform "https://%h/%o/%n/pulls/%i#issuecomment-%I")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/branch/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/pulls") ; sic
   (pullreq-refspec           :initform "+refs/pull/*/head:refs/pullreqs/*")))

;;; Pull

(cl-defmethod forge--pull ((repo forge-gitea-repository) until)
  )

(cl-defmethod forge--update-repository ((repo forge-gitea-repository) data)
  (let-alist data
    ; (oset repo )
    ))

;;; Issues

(cl-defmethod forge--fetch-issues ((repo forge-gitea-repository) callback until)
  (let ((state)
        (page)
        (issues)
        (issue-tail)
        (issue)
        (issue-count 0)
        (issue-index 0)
        (then))
    (cl-labels ((fetch-issues (state* then*)
                  (setf state state*
                        page 1
                        then then*)
                  (fetch-page))
                (fetch-page ()
                  (message "page %s" page)
                  (cl-incf page)
                  (forge--gtea-get repo "repos/:project-keep-slash/issues"
                                   `((page  . ,(1- page))
                                     (state . ,state))
                                   :callback #'on-issues))
                (on-issues (value _headers _status _req)
                  (if (null value)
                      (funcall then)
                    (setf issues (append issues value))
                    (cl-incf issue-count (length value))
                    (setf issue-tail value)
                    (next-issue)))
                (next-issue ()
                  (if issue-tail
                      (progn
                        (setf issue (pop issue-tail))
                        (cl-incf issue-index)
                        (message "%s issue %s/%s" state issue-index issue-count)
                        (forge--msg nil nil nil "Pulling issue %s/%s"
                                    issue-index issue-count)
                        (forge--fetch-issue-posts repo issue-index #'on-posts))
                    (fetch-page)))
                (on-posts (value)
                  (setf (alist-get 'comments issue) value)
                  (next-issue)))
      (fetch-issues "closed" (lambda () (fetch-issues "open" (lambda () (funcall callback issues))))))))

(cl-defmethod forge--fetch-issue-posts ((repo forge-gitea-repository) cur callback)
  (forge--gtea-get repo
                   (format "/repos/:project-keep-slash/issues/%s/comments" cur)
                   '()
                   :callback (lambda (value _headers _status _req)
                               (funcall callback value))))

(cl-defmethod forge--update-issue ((repo forge-gitea-repository) data)
  (let-alist data
    (let* ((issue-id (forge--object-id 'forge-issue repo .id))
           (issue (forge-issue
                   :id         issue-id
                   :repository (oref repo id)
                   :number     .id
                   :state      (pcase-exhaustive .state
                                 ("open"   'open)
                                 ("closed" 'closed))
                   :author     .user.login
                   :title      .title
                   :created    .created_at
                   :updated    .updated_at
                   :closed     .closed_at
                   :milestone  .milestone.id
                   :body       (forge--sanitize-string .body))))
      (closql-insert (forge-db) issue t)
      (unless (magit-get-boolean "forge.omitExpensive")
        (let ((assignee-ids (mapcar (lambda (assignee) (alist-get 'id assignee))
                                    .assingnees)))
          (forge--set-id-slot repo issue 'assignees .assignees))
        (let ((label-ids (mapcar (lambda (label) (alist-get 'id label))
                                 .labels)))
          (forge--set-id-slot repo issue 'labels label-ids)))
      (dolist (comment .comments)
        (let-alist comment
          (let ((post
                 (forge-issue-post
                  :id      (forge--object-id issue-id .id)
                  :issue   issue-id
                  :number  .id
                  :author  .user.login
                  :created .create_at
                  :updated .updated_at
                  :body    (forge--sanitize-string .body))))
            (closql-insert (forge-db) post t)))))))

;;; Other

(cl-defmethod forge--fetch-assignees ((repo forge-gitea-repository) callback)
  (cl-labels ((on-members (value)
                          )
              (on-collaborators (value)
                                ))
    (forge--gtea-get repo "/orgs/:owner/members"
                     '()
                     :callback (lambda (value _headers _status _req)
                                 (message "%s" value)
                                 (funcall callback value)))
    (when nil (forge--gtea-get repo "/repos/:project-keep-slash/collaborators"
                      '()
                      :callback (lambda (value _headers _status _req)
                                  (message "%s" value)
                                  (funcall callback value))))))

(cl-defmethod forge--update-assignees ((repo forge-gitea-repository) assignees)
  (let ((repo-id (oref repo id)))
    (oset repo assignees
          (mapcar (lambda (assignee)
                    (let-alist assignee
                      (list (forge--object-id repo-id .id)
                            .login
                            .full_name
                            .id)))
                  assignees))))

(cl-defmethod forge--fetch-labels ((repo forge-gitea-repository) callback)
  ;; FIXME fetch all pages
  (forge--gtea-get repo "/repos/:project-keep-slash/labels"
    '()
    :callback (lambda (value _headers _status _req)
                (message "%s" value)
                (funcall callback value))))

(cl-defmethod forge--update-labels ((repo forge-gitea-repository) labels)
  (let ((repo-id (oref repo id)))
   (oset repo labels
         (mapcar (lambda (label)
                   (let-alist label
                     (list (forge--object-id repo-id .id)
                           .name
                           (format "#%s" .color)
                           ;; Gitea doesn't seem to have descriptions
                           "")))
                 labels))))

;;; Utilities

(cl-defun forge--gtea-get (obj resource &optional params &key callback host)
  (gtea-get (if obj (forge--format-resource obj resource) resource)
            params
            :host     (or host (oref (forge-get-repository obj) apihost))
            :auth     'forge
            :callback callback))

(provide 'forge-gitea)
;;; forge-gitea.el ends here
