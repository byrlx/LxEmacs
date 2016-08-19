;;我的emacs环境入口
;;Author: luis404(luisxu404@gmail.com)

;; === 配置远程仓库 =====
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; 设置backup路径 ~文件的存放地
(add-to-list 'load-path "~/.emacs.d/download/")
(add-to-list 'load-path "~/.emacs.d/lx/") ;;my custome folder

;; 使用emacs做ppt 
;; org-reveal
(add-to-list 'load-path "/Users/douhua/lx/emacs/org-reveal")
(require 'ox-reveal)
(setq org-reveal-root "/Users/douhua/lx/emacs/reveal.js")

;;设置语言环境
(setenv "LANG" "en_US.UTF-8")

;; version control, 将文件的版本信息放到这个目录

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)   
  (setq backup-directory-alist
          `((".*" . ,"~/.emacs.d/backup")))
    (setq auto-save-file-name-transforms
          `((".*" ,"~/.emacs.d/backup" t)))

;; require
; ========= 全局变量 =========
(defvar LXBASEDIR "~/lx/GoogleDrive/lxorg/")
(defvar LXBLOG "~/lx/emacs/blog/output/")
(defvar LXLEARN (concat LXBASEDIR "learn/"))
(defvar LXBLOGORG "~/lx/emacs/blog/src/")

;;高亮括号
(show-paren-mode 1)
;; 自动删除备份
(setq delete-old-versions t)
;;===========日历配置=============
(setq calendar-remove-frame-by-deleting t)

(setq general-holidays nil)
(setq all-christian-calendar-holidays nil)
(setq all-hebrew-calendar-holidays nil)
(setq all-islamic-calendar-holidays nil)

(setq diary-file (concat LXBASEDIR "emacs-diary"))
(setq appt-audible t)
(setq appt-display-mode-line t)
(setq appt-display-duration (* 60 60 1))
(setq appt-message-warning-time 10)

(setq mark-diary-entries-in-calendar t)       ; 标记有记录的日子  
(setq mark-holidays-in-calendar nil)          ; 标记节假日  
(setq view-calendar-holidays-initially nil)   ; 不显示节日列表
;;===========日历配置=============

;;=========== Org mode 配置 =============
;;include emacs diary to org agenda
(setq org-agenda-include-diary t)

(appt-activate 1)
(org-agenda-to-appt)

(setq-default org-catch-invisible-edits 'error)

;;设置TODO依赖
(setq org-enforce-todo-dependencies t) 
;;设置checkbox依赖
(setq org-enforce-todo-checkbox-dependencies t)
;;设置递归子树统计
(setq org-hierarchical-todo-statistics t)

(setq org-log-done 'time)

;;子任务全部完成后父任务设为完成,有一个bug: 子任务修改状态时会自动改到根祖先的状态.
;; (defun org-summary-todo (n-done n-not-done)
  ;;"Switch entry to DONE when all subentries are done, to TODO otherwise."
;; (let (org-log-done org-log-states)	; turn off logging
;;  (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;;(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;设置org capture文件
(define-key global-map "\C-cp" 'org-capture)
(setq org-capture-templates
      '(("t" "Coohua todo" entry (file+headline (concat LXLEARN "coohua.org") "Todo")  "* TODO  %? %T")
	("l" "Global Note" entry (file+headline (concat LXBLOGORG "Other/2016-01-22-bookmarks.org") "Links") "**** %?")
	("n" "Coohua tasks" entry (file+headline (concat LXLEARN "coohua.org") "Note")  "* %? %T")
	("e" "Emacs Tips" entry (file+headline (concat LXBLOGORG "Emacs/2016-01-22-Emacs-Little-Tips.org") "Tips")  "* %?")
        ("k" "Local Tips" entry (file+headline (concat LXLEARN "all.org") "Tips")  "* %?\n SCHEDULED: %T\n %a")
        ("p" "TODO" entry (file+headline (concat LXLEARN "all.org") "Tasks")  "* TODO %?\n SCHEDULED: %T\n %a")))

(setq org-feed-alist
      '(("coolshell"
	 "http://coolshell.cn/feed"
	 "~/lx/feeds.org" "coolshell")))

;;设置agenda的收集目录
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list
			(concat LXLEARN "all.org")
			(concat LXLEARN "coohua.org")
			(concat LXBASEDIR "security/note.org")))
;;设置agenda的显示时间
(setq org-agenda-span 10) 
;;设置起始日
(setq org-agenda-start-day "-1d")

;; =============== Org Publish =====================
(setq org-publish-project-alist
      '(("github-blog"
	 :base-directory "~/lx/blog/org/"
	 :publishing-directory "~/lx/blog/_posts/"
	 :prepare-function nil
	 :completion-function nil
	 :exclude "*.html"
	 :recursive t
	 :base-extension "org"
	 :publishing-function org-html-publish-to-html
	 :section-number nil
	 :headline-levels nil
	 :body-only t
	 )))

(defun lx-public-blog nil
  "Publish my github blog"
  (interactive)
  (org-publish))

(setq org-todo-keyword-faces
      '(("ABORT" . (:box (:line-width 1 :color "gray31") :foreground "yellow" ))))
;; =============== Org mode 配置 =====================

;; ============== 自定义快捷函数 ====================
;; ------------ 打开coohua文件 -------------------
(defun lx-open-coohua ()
  (interactive)
  (org-agenda-to-appt)
  (find-file (concat LXLEARN "coohua.org")))

;; ------------ 打开coohua magit buffer ------------ 
(defun lx-magit-ch ()
  (interactive)
  (magit-status "~/coohua/Coohuaclient"))

;; --- 自动创建blog文章, 创建完成后自动插入头部 ---
;; 参数: 日期, 题目, category
(defun lx-create-post-today (title category)
  (interactive "sInput title:\nsInput category: ")
  (lx-create-post (format-time-string "%Y-%m-%d") title category))
(defun lx-create-post (date title category)
  (interactive "sInput date:\nsInput title:\nsInput category:")
  ;; 组装文件名
  (setq whole-filename (concat LXBLOGORG category "/" date "-" title ".org"))
  ;; 创建文件, 打开文件
  (setq filebuf (find-file-other-window whole-filename))
  ;; 插入通用头部
  (lx-create-post-head title category)
  ;; 光标定位在头部下边的空白行
  )

;; 根据title和category生成头部
(defun lx-create-post-head (title category)
  ;;读取头部文件内容并插入到文件开始
  (insert-file-contents (concat LXBLOGORG "header"))
  ;;移动光标, 填写标题和category
  (goto-char (point-max))
  (forward-line -3)
  (end-of-line)
  (insert (concat "[" category "]"))
  (forward-line -1)
  (end-of-line)
  (insert title)
  )

;;(org-babel-lob-ingest "/tmp/lxbabel.org")
(require 'org-mac-iCal)
;; --- END 自动创建blog文章, 创建完成后自动插入头部 ---

  ;; Gnus配置
(setq gnus-select-method '(nnmaildir "Gmail" (directory "~/lx/Gmail")))
;;(setq gnus-select-method '(nntp "news.gmane.org"))
;; ============== 自定义快捷函数 ====================


;;Clojure配置
(add-hook 'clojure-mode-hook 'paredit-mode)
;;================= Clojure ==============

(defun lx-learn ()
  (interactive)
  (dired LXLEARN))

;; 自己写的小函数
(require 'lxlab)

;; Emacs color theme file
;;(require 'color-theme)
;;(color-theme-initialize)
;;(color-theme-xemacs)
;; Init xcscope
(require 'xcscope)
(setq package-enable-at-startup nil)
(package-initialize)

(setq gtags-suggested-key-mapping t)

;;(require 'e2wm)

;;设置字体编码
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

;; 该插件实现中英文对齐
(require 'chinese-fonts-setup) ;; chinese font set

;; 让eshell使用shell的PATH环境
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; lisp mode 自动补全
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; 自动在文件后面加入新行
(setq require-final-newline t)

;; 加载主题
(setq custom-theme-directory "~/.emacs.d/lx/themes/")
(load-theme 'zenburn t)

;; 配置evil mode, 使用vim快捷键, 依赖lx-init
(require 'evil) 
(evil-mode 1)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'magit-mode 'emacs)

;; 开启column mode, 在Mode Line显示列号
(column-number-mode 1)
;; 设置文件后缀与mode相关联
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("build.gradle" . groovy-mode))

;; paredit, 实现lisp编程括号自动补全
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; 高亮当前行
(global-hl-line-mode 1)
;; (custom-set-faces
;; '(highlight ((t (:background "grey80" :foreground "black" :bold t)))) ;; highlight current line


;; smex实现命令自动补全, 依赖lx-init, 
(require 'smex) 
(ido-mode t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; 系统自动设置的配置
(setq window-numbering-assign-func
      (lambda () (when (equal (buffer-name) "*Calculator*") 9)))

;; slime to function definition
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; LaTex中文支持
;; (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
	;;		      "xelatex -interaction nonstopmode %f"))

;; 打开magit的help buffer
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; 使用worg
(require 'htmlize)
;; to have things work correctly in batch-mode
(require 'font-lock)
(require 'cc-mode)
(c-after-font-lock-init)

(setq make-backup-files nil
      vc-handled-backends nil)

(setq org-export-default-language "en"
      org-export-html-extension "html"
      org-export-with-timestamps nil
      org-export-with-section-numbers nil
      org-export-with-tags 'not-in-toc
      org-export-skip-text-before-1st-heading nil
      org-export-with-sub-superscripts '{}
      org-export-with-LaTeX-fragments t
      org-export-with-archived-trees nil
      org-export-highlight-first-table-line t
      org-export-latex-listings-w-names nil
      org-html-head-include-default-style nil
      org-html-head ""
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-publish-list-skipped-files t
      org-publish-use-timestamps-flag t
      org-export-babel-evaluate nil
      org-confirm-babel-evaluate nil)

(eval-after-load "org-html"
'(setq org-html-scripts
       (concat org-html-scripts "\n"
	       "<script type=\"text/javascript\">
    function rpl(expr,a,b) {
      var i=0
      while (i!=-1) {
         i=expr.indexOf(a,i);
         if (i>=0) {
            expr=expr.substring(0,i)+b+expr.substring(i+a.length);
            i+=b.length;
         }
      }
      return expr
    }

    function show_org_source(){
       document.location.href = rpl(document.location.href,\"html\",\"org.html\");
    }
</script>
")))

;; re-export everything regardless of whether or not it's been modified
(setq org-publish-use-timestamps-flag nil)

(setq worg-base "~/lx/emacs/blog/src/")
(setq worg-htmlroot "~/lx/emacs/blog/output/")
(setq worg-base-directory worg-base)
(setq worg-base-style-directory (concat worg-base "style/"))
(setq worg-base-code-directory (concat worg-base "code/"))
(setq worg-base-color-themes-directory (concat worg-base "color-themes/"))
(setq worg-base-img-directory (concat worg-base "img/"))
(setq worg-publish-directory worg-htmlroot)
(setq worg-publish-img-directory (concat worg-htmlroot "img/"))
(setq worg-publish-style-directory (concat worg-htmlroot "style/"))

(defun set-org-publish-project-alist ()
  "Set publishing projects for Orgweb and Worg."
  (interactive)
  (setq org-publish-project-alist
	`(("worg" :components ("worg-pages" "worg-images" "worg-extra" ))
	  ("worg-pages"
	   :base-directory ,worg-base-directory
	   :base-extension "org"
	   :exclude "dont_publish"
	   :makeindex nil
	   :auto-sitemap t
	   :sitemap-filename "index.org"
	   :sitemap-title "Luis's Blog"
	   :sitemap-ignore-case t
	   :html-extension "html"
	   :publishing-directory ,worg-publish-directory
	   :publishing-function (org-html-publish-to-html)
	   :htmlized-source t
	   :section-numbers nil
	   :table-of-contents nil
	   :html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"/style/worg.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Zenburn\" href=\"/style/worg-zenburn.css\" type=\"text/css\" />
<link rel=\"alternate stylesheet\" title=\"Classic\" href=\"/style/worg-classic.css\" type=\"text/css\" />
<link rel=\"SHORTCUT ICON\" href=\"/org-mode-unicorn.ico\" type=\"image/x-icon\" />
<link rel=\"icon\" href=\"/org-mode-unicorn.ico\" type=\"image/ico\" />"
	   :recursive t
	   :html-preamble ,(with-temp-buffer (insert-file-contents "~/lx/emacs/worg/preamble.html") (buffer-string))
	   :html-postamble "<p class=\"postamble\">Created At %d by %a. Email: %e</p>"
	   )
	  ("worg-images"
	   :base-directory ,worg-base-img-directory
	   :base-extension "png\\|jpg\\|gif\\|pdf\\|csv\\|css\\|tex"
	   :publishing-directory ,worg-publish-img-directory
	   :recursive t
	   :publishing-function org-publish-attachment)
	  ("worg-extra"
	   :base-directory ,worg-base-style-directory
	   :base-extension "css"
	   :publishing-directory ,worg-publish-style-directory
	   :publishing-function org-publish-attachment)
	  )))

(set-org-publish-project-alist)

(defun worg-fix-symbol-table ()
  (when (string-match "org-symbols\\.html" buffer-file-name)
    (goto-char (point-min))
    (while (re-search-forward "<td>&amp;\\([^<;]+;\\)" nil t)
      (replace-match (concat "<td>&" (match-string 1)) t t))))

(defun publish-worg nil
   "Publish Worg."
   (interactive)
   (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
   (let ((org-format-latex-signal-error nil)
	 (worg-base-directory worg-base)
	 (worg-base-code-directory (concat worg-base "code/"))
	 (worg-base-color-themes-directory (concat worg-base "color-themes/"))
	 (worg-base-images-directory (concat worg-base "images/"))
	 (worg-publish-directory worg-htmlroot))
     (set-org-publish-project-alist)
     (message "Emacs %s" emacs-version)
     (org-version)
     (org-publish-project "worg")))

(defun publish-orgweb nil
   "Publish Org web pages."
   (interactive)
   (add-hook 'org-publish-after-export-hook 'worg-fix-symbol-table)
   (let ((org-format-latex-signal-error nil)
	 (org-export-with-sub-superscripts nil))
     (set-org-publish-project-alist)
     (org-publish-project "orgweb")))

(defun parse-org-quotes ()
  "Create ~/orgmode.org/org-quotes.js from org-quotes.org."
  (interactive)
  (load (concat worg-base "code/elisp/worg-fortune.el"))
  (worg-write-fortune-file
   (concat worg-base "org-quotes.org")
   "~/orgmode.org/org-quotes.js"
   120
   "r_text[%d] = \"%s\";" "\n"
   'worg-fortune-insert-javascript-pre
   'worg-fortune-insert-javascript-post))

