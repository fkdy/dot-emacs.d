;;
;; config elpy
;;

;;(mel/require-package 'elpy)

(use-package elpy
  :defer t
  :init
  (with-eval-after-load 'python
    (elpy-enable))
  :config
  (setq elpy-rpc-virtualenv-path
        (expand-file-name "emacs" (mel/expand-auto-dir "elpy")))
  ;; set $WORKON_HOME env variable to the directory containing all the
  ;; virtual env, so pyvenv-workon can select which virutal env to use.
  (setenv "WORKON_HOME" (file-name-directory elpy-rpc-virtualenv-path))
  (setq elpy-rpc-python-command "python")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i")
  (when (file-directory-p elpy-rpc-virtualenv-path)
    ;; the 'emacs' virtualenv is the default virtualenv(assume the
    ;; 'emacs' virutalenv has been installed in
    ;; `(mel/expand-auto-dir "elpy/emacs")'
    (pyvenv-workon "emacs"))
  (when (eq system-type 'windows-nt)
    ;; shell completion may not work properly on windows.
    (setq python-shell-completion-setup-code
          "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        from sys import platform
        if platform == 'win32':
            from pyreadline3 import Readline
            readline = Readline()
        else:
            import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins
                      or '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = rlcompleter.Completer().complete
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions")))

(provide 'conf-elpy)
