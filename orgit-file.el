;;; orgit-file.el --- Support for links to files in Git repositories  -*- lexical-binding:t -*-

;; Author: Gino Cornejo
;; mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/orgit-file
;; Keywords: hypermedia vc

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (compat "30.1") (orgit "2.0") (org "9.7"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package defines the Org link type `orgit-file', which can be
;; used to link to files in Git repositories at specific revisions.
;;
;; The package integrates with Magit to allow storing links from both
;; regular file buffers and Magit blob-mode buffers (when viewing
;; historical file revisions).
;;
;; Format
;; ------
;;
;; The link type defined here takes this form:
;;
;;    orgit-file:/path/to/repo/::REV::FILE  links to FILE at REV
;;
;; You can optionally add a search option after FILE:
;;
;;    orgit-file:/path/to/repo/::REV::FILE::SEARCH
;;
;; Where SEARCH is any Org search string (e.g., a heading, custom ID,
;; line number, or regex).
;;
;; Export
;; ------
;;
;; When an Org file containing such links is exported, then the url of
;; the remote configured with `orgit-remote' is used to generate a web
;; url according to `orgit-export-alist'.  That webpage should present
;; the file content at the specified revision.
;;
;; Both the remote to be considered the public remote, as well as the
;; actual web urls can be defined in individual repositories using Git
;; variables.
;;
;; To use a remote different from `orgit-remote' but still use
;; `orgit-export-alist' to generate the web urls, use:
;;
;;    git config orgit.remote REMOTE-NAME
;;
;; To explicitly define the web url for files, use something like:
;;
;;    git config orgit.file http://example.com/repo/blob/%r/%f
;;
;; Where %r is replaced with the revision and %f with the file path.

;;; Code:

(require 'compat)
(require 'orgit)

(unless (fboundp 'org-link-store-props)
  (defalias 'org-link-store-props 'org-store-link-props))

(eval-when-compile (require 'subr-x))

;;; Options

(defgroup orgit-file nil
  "Org links to files in Git repositories."
  :group 'magit-extensions
  :group 'org-link)

(defcustom orgit-file-abbreviate-revisions t
  "Whether to use abbreviated revision hashes in stored links.

When non-nil, use `magit-rev-abbrev' to shorten revision hashes
in stored links.  When nil, use full 40-character SHA-1 hashes.

Abbreviated hashes are more readable but may become ambiguous in
very large repositories with many commits."
  :group 'orgit-file
  :type 'boolean)

;;; File links

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit-file"
                             :store    #'orgit-file-store
                             :follow   #'orgit-file-open
                             :export   #'orgit-file-export
                             :complete #'orgit-file-complete-link)))

;;;###autoload
(defun orgit-file-store (&optional _interactive?)
  "Store a link to the file in a Magit file or blob buffer.

The link includes the repository, revision, and file path.

When in a `magit-blob-mode' buffer (viewing a historical
revision), store a link to that specific revision.

When in a regular file buffer within a Git repository, store a
link to the file at the current HEAD revision.

Return non-nil if a link was stored, nil otherwise."
  (when-let* ((repo (magit-toplevel))
              (file (or (and (derived-mode-p 'magit-blob-mode)
                             magit-buffer-file-name)
                        (magit-file-relative-name)))
              (rev (or (and (derived-mode-p 'magit-blob-mode)
                            magit-buffer-revision)
                       (and buffer-file-name
                            (magit-rev-parse "HEAD")))))
    (let* ((repo-id (orgit--current-repository))
           (rev-display (if orgit-file-abbreviate-revisions
                            (magit-rev-abbrev rev)
                          rev))
           (link (format "orgit-file:%s::%s::%s" repo-id rev file))
           (description (format "%s@%s:%s"
                                (file-name-nondirectory file)
                                rev-display
                                repo-id)))
      (org-link-store-props
       :type "orgit-file"
       :link link
       :description description)
      ;; Return non-nil to indicate we handled this
      t)))

;;;###autoload
(defun orgit-file-open (path)
  "Open orgit-file link at PATH.

PATH format: REPO::REV::FILE-PATH or REPO::REV::FILE-PATH::SEARCH

Navigate to the specified file at the given revision in the
repository.  If SEARCH is provided, search for that string or
pattern in the file after opening."
  (pcase-let* ((`(,repo ,rev ,file-path ,search-option)
                (orgit-file--parse-path path))
               (default-directory (orgit--repository-directory repo)))
    (magit-find-file rev file-path)
    (when search-option
      (org-link-search search-option))))

;;;###autoload
(defun orgit-file-export (path desc format)
  "Export orgit-file link at PATH to web URL if possible.

DESC is the link description.  FORMAT is the export backend.

Use `orgit-export-alist' to generate web URLs for known Git
hosting services.  The remote used is determined by `orgit-remote'
or the Git variable `orgit.remote'.

Return formatted link for FORMAT, or signal `org-link-broken' if
the URL cannot be determined."
  (pcase-let* ((`(,repo ,rev ,file-path ,_search-option)
                (orgit-file--parse-path path))
               (dir (orgit--repository-directory repo)))
    (if (file-exists-p dir)
        (let* ((default-directory dir)
               (remotes (magit-git-lines "remote"))
               (remote  (magit-get "orgit.remote"))
               (remote  (cond ((length= remotes 1) (car remotes))
                              ((member remote remotes) remote)
                              ((member "origin" remotes) "origin"))))
          (if remote
              (if-let ((link
                        (or (and-let* ((url (magit-get "orgit" "file")))
                              (format-spec url `((?r . ,rev)
                                                 (?f . ,file-path))))
                            (and-let* ((url (magit-get "remote" remote "url"))
                                       (template (cl-find-if
                                                  (lambda (elt)
                                                    (string-match (car elt) url))
                                                  orgit-export-alist)))
                              (format-spec (nth 1 template)
                                           `((?n . ,(match-string 1 url))
                                             (?r . ,rev)
                                             (?f . ,file-path)))))))
                  (orgit--format-export link desc format)
                (signal 'org-link-broken
                        (list (format "Cannot determine public url for %s"
                                      path))))
            (signal 'org-link-broken
                    (list (format "Cannot determine public remote for %s"
                                  default-directory)))))
      (signal 'org-link-broken
              (list (format "Cannot determine public url for %s %s"
                            path "(which itself does not exist)"))))))

;;;###autoload
(defun orgit-file-complete-link (&optional arg)
  "Complete orgit-file link with repository, revision, and file selection.

Optional ARG is passed to `magit-read-repository'.

Prompt for a repository, then a revision (branch, tag, or commit),
then a file from that revision.  Return a complete orgit-file link."
  (let* ((default-directory (magit-read-repository arg))
         (repo (orgit--current-repository))
         (rev (magit-read-branch-or-commit "Revision"))
         (file (magit-read-file-from-rev rev "File")))
    (format "orgit-file:%s::%s::%s" repo rev file)))

;;; Utilities

(defun orgit-file--parse-path (path)
  "Parse orgit-file link PATH.

PATH format: REPO::REV::FILE-PATH or REPO::REV::FILE-PATH::SEARCH

Return (REPO REV FILE-PATH SEARCH-OPTION) as a list."
  (let* ((parts (split-string path "::" t))
         (repo (nth 0 parts))
         (rev (nth 1 parts))
         (file-path (nth 2 parts))
         (search-option (nth 3 parts)))
    (list repo rev file-path search-option)))

(provide 'orgit-file)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orgit-file.el ends here
