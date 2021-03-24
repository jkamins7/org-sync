;;; org-sync-forge.el --- backend for org-sync to projects using forge
;;
;; Copyright (C) 2021 jkaminsky
;;
;; Author: jkaminsky <jkaminsky at jhu dot edu>
;; Keywords: org, github, synchronization
;; Homepage: https://github.com/jkamins7/org-sync
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package implements a backend for org-sync to synchnonize
;; issues from an emacs forge database with an org-mode buffer.  Read
;; Org-sync documentation for more information about it.
;;
;; This backend supports basic bug synching along with tag creation.
;; If you add or change the tags of an issue to something that doesn't
;; exists, it will be created.
;;
;;; Code:

(require 'cl-lib)
(require 'org-sync)
(require 'forge)

(defvar org-sync-forge-backend
  '((base-url      . org-sync-forge-base-url)
    (fetch-buglist . org-sync-forge-fetch-buglist)
    (send-buglist  . org-sync-forge-send-buglist))
  "Forge backend.")

(defun org-sync-forge-time-to-string (time)
  "Return TIME as a full ISO 8601 date string, but without timezone adjustments (which forge doesn't support"
  (format-time-string "%Y-%m-%dT%TZ" time t))

(defun org-sync-forge-parse-url (url)
  "Return base url from given url URL."
  ;; Fix me to actually use a repo name here :

  (when (string-match "forge/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)" url)
    (let ((host (match-string 1 url))
	  (user (match-string 2 url))
          (repo (match-string 3 url)))
      (list host user repo))))

(defun org-sync-forge-base-url (url)
  "Return base url from given url URL."
  (let* ((parsed-url (org-sync-forge-parse-url url)))
    (when parsed-url (reduce 'concat (append (list "forge") (mapcar (lambda (x) (concat "/" x)) parsed-url))))))


(defun org-sync-forge-sql-query-string ()
  "SELECT
     id,
     author,
     assignees,
     state,
     title,
     note,
     milestone,
     labels,
     created,
     updated
   FROM
     issue"
  )

(defun org-sync-forge-fetch-buglist (last-update)
  "Return the buglist pulled from forge's database with entries that have changed since LAST-UPDATE."
  (let* ((updated_recently (when last-update
                             (format " WHERE datetime(substr(issue.updated,2,20)) >= datetime(\"%s\")" (org-sync-forge-time-to-string last-update))))
         (sql_query (concat (org-sync-forge-sql-query-string) updated_recently))
         (sql_results (forge-sql sql_query))
         (title "Issues from forge")
         )
    `(:title ,title
             :url ,org-sync-base-url
             :bugs ,(mapcar 'org-sync-forge-sql-result-to-bug sql_results)
             :since ,last-update))
  )

(defun org-sync-forge-sql-result-to-bug (sql-result)
  "Turn a single row of the forge SQL-RESULT into a bug."
    (list
     :id (nth 0 sql-result)
     :author (nth 1 sql-result)
     :assignee (nth 2 sql-result)
     :status (nth 3 sql-result)
     :title (nth 4 sql-result)
     :desc (nth 5 sql-result)
     :milestone (nth 6 sql-result)
     :tags (nth 7 sql-result)
     :date-creation (org-sync-parse-date (nth 8 sql-result))
     :date-modification (org-sync-parse-date (nth 9 sql-result))
    )
  )

(defun org-sync-forge-send-buglist (buglist)
  "Send a BUGLIST to forge and return new bugs."
  (let* (newbugs)
  (dolist (b (org-sync-get-prop :bugs buglist))
    (let* ((id (org-sync-get-prop :id b))
	   (sync (org-sync-get-prop :sync b))
	   )
      (cond
       ;; new bug
       ((null id)
	(org-sync-forge-create-bug b org-sync-base-url))

       ;; update bug
       ((forge-get-topic id)
	(org-sync-forge-update-bug b (forge-get-topic id) url))

       ;; malformed bug
       (t
	(error "The id provided does not exist in database"))
       ))

    ;;      (when (stringp err)
    ;;        (error "Github: %s" err))))
    )
  `(:bugs ,newbugs))
  )

(defun org-sync-forge-create-bug (bug url)
  "Create an issue for a forge named URL from a new BUG."
  (let* (
	 (.title (org-sync-get-prop :title bug))
	 (.body (org-sync-get-prop :desc bug))
	 (.labels (org-sync-get-prop :tags bug))
	 (.assignees (org-sync-get-prop :assignee bug))
	 (.state (org-sync-get-prop :status bug))
	 )
    (forge--ghub-post (forge-get-repository (org-sync-forge-parse-url url)) "/repos/:owner/:repo/issues"
      `((title . , .title)
	(body  . , .body)
	,@(and .labels    (list (cons 'labels    .labels)))
	,@(and .assignees    (list (cons 'assignees    .assignees)))
	;;,@(and .state    (list (cons 'state    .state)))
	)
      ;; :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback)
      )
    ))

(defun org-sync-forge-update-bug (new-bug existing-bug url)
  "Update an EXISTING-BUG with new data from a NEW-BUG storing results in URL."
  (let* (
	 (.title (org-sync-get-prop :title bug))
	 (.body (org-sync-get-prop :desc bug))
	 (.labels (org-sync-get-prop :tags bug))
	 (.assignees (org-sync-get-prop :assignee bug))
	 (.state (org-sync-get-prop :status bug))
	 )
    (forge--ghub-patch existing-bug "/repos/:owner/:repo/issues/:number"
      `((title . , .title)
	(body  . , .body)
	,@(and .labels    (list (cons 'labels    .labels)))
	,@(and .assignees    (list (cons 'assignees    .assignees)))
	;;,@(and .state    (list (cons 'state    .state)))
	)
      ;; :callback  (forge--post-submit-callback)
      :errorback (forge--post-submit-errorback)
      )
    )
  )

(provide 'org-sync-forge)
;;; org-sync-forge.el ends here
