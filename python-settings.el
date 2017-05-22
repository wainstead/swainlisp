;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; originally from swainlib.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; python mode stuff, if I have it
;; (load "python-mode")
;; (setq auto-mode-alist
;;       (append '(
;;                 ("\\.py\\'" . python-mode)) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; originally from dotemacs.nfmc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load a newer version of python-mode
;; (add-to-list 'load-path "~/toolbox/external_packages/python-mode.el-6.0.4/") 
;; (setq py-install-directory "~/toolbox/external_packages/python-mode.el-6.0.4/")
;; (require 'python-mode)

;; attempt to get Emacs's "M-x compile" to work with the newly merged
;; in "unification" branch, 2/23/2012
(setenv "PYTHONPATH" (shell-command-to-string "source ~swain/.bashrc; source ~/.vpy/default/bin/activate; echo -n $PYTHONPATH"))

;; Copied from python.el that came with Emacs 24.3.1
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; 
;; (load-file "~swain/.emacs.d/ipython.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; originally from ssc.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert-tabs-hook-func defined in ssc.el
;; (add-hook 'python-mode-hook     'insert-tabs-hook-func)

;; don't add trailing whitespace
(add-hook 'python-mode-hook
		  (lambda ()
			(setq show-trailing-whitespace t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; originally from ~/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq py-install-directory "~swain/.emacs.d/python-mode.el-6.2.0/")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new stuff (based on instructions in python.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; obsolete in 25.2..?
;(setq python-shell-virtualenv-path "~swain/.vpy/27production0/")

(setq python-shell-virtualenv-root "~swain/.vpy/27production0/")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
