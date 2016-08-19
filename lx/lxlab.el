;; Author: luis404(luisxu404@gmail.com)

;; 设置org文件相关的全局路径名
(setq ORG-PATH "~/lx/GoogleDrive/lxorg/learn/")

;;;Below is some handy functions
;; ===========================================
;; open coohua code with magit
(defun lx-magit-my-ch ()
  (interactive)
  (magit-status "~/coohua/my-forked-branches/CHClient"))

;; reload emacs file after change
(defun lx-reload ()
  (interactive)
  (load-file "~/.emacs"))

;; insert author info in the current point
(defun lx-insert-author ()
  (interactive)
  (insert "Author: " "luis404" "(luisxu404@gmail.com)"))

(defun lxel ()
  "This is lx's elisp file"
  (interactive)
  (find-file "~/.emacs.d/lx/lxlab.el"))

(defun lx-M ()
  (interactive)
  (setq items (lx-trim-list
	       (split-string (lx-get-line) "[ ]+")))
  (setq sum 0)
  (while items
    (setq item (car items))
    (if (string-match-p "/" item) 
	nil
      (setq sum (+ sum (string-to-number item))))
    (setq items (cdr items)))
  (insert (concat (format "%.2f" sum "|"))))
;; =============== Handy Funcs =====================

;; ========== 封装magit,显示指定目录=========
(defun lxgit (lxdir)
  (interactive "DInput directory: ")
  (magit-status lxdir))


;; ========== 自动添加跑步记录 ================
(defun lx-add-running (day km loc)
  (interactive "sInput day: \nsInput distance: \nsInput location: ")
  (lx-add-running-record  day km loc))

(defun lx-add-running-record (day km loc)
  (let  ((record-file (concat LXBLOGORG "Other/2015-01-19-Running-Record.org")))
    (find-file record-file)
    (with-current-buffer "2015-01-19-Running-Record.org"
      (search-forward (format-time-string "%Y"))
      (forward-line 3)
      (org-table-insert-row)
      (insert day)
      (org-table-next-field)
      (insert (message "%.1f" (string-to-number km)))
      (org-table-next-field)
      (insert loc)
      (org-table-align)
      (save-buffer)
      (kill-this-buffer)))
  (lx-update-blog))

;;自动更新blog
(defun lx-update-blog ()
  (interactive)
  (org-publish "worg")
  (shell-command (concat "cd " LXBLOG " && git add -A && git ci -m 'update blog' && git push"))
  (shell-command (concat "cd " LXBLOGORG " && git add -A && git ci -m 'update blogsrc' && git push")))
(global-set-key (kbd "<f9>") 'lx-update-blog)

(defun lx-add-running-today (km loc)
  (interactive "sInput distance: \nsInput location: ")
  (lx-add-running-record (format-time-string "%m.%d") km loc))
;; ============== 代码学习实验 ================

(provide 'lxlab)
