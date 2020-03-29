
# Table of Contents

1.  [emacs configuration](#org567c49c)
        1.  [emacs.d](#org71a5a89)


<a id="org567c49c"></a>

# emacs configuration


<a id="org71a5a89"></a>

### emacs.d

目录结构：

.emacs.d
├── readme.org
├── init.el
└── pkgs-config
    ├── conf-ace-jump-mode.el
    ├── conf-elpy.el
    ├── conf-ggtags.el
    ├── conf-packages.el
    ├── conf-preference.el
    ├── conf-smex.el
    ├── conf-undo-tree.el
    └── conf-use-package.el

1.  pkgs

    pkgs 文件夹中的文件由 melpa 或者 gnu 上下载到本地的 elisp 包，每个
    版本的 emacs 会有一个单独的文件夹，参考 [conf-package.el](file:///home/mel/.emacs.d/pkgs-config/conf-packages.el) 第 8 行：
    
        ;; package dirs
        (let ((versioned-pkg-dirs
               (expand-file-name
                (format "pkgs/elpa-%s.%s" emacs-major-version emacs-minor-version)
                user-emacs-directory)))
          (setq package-user-dir versioned-pkg-dirs))
    
    当前 packages 由 163.com 下载，参考 [conf-package.el](file:///home/mel/.emacs.d/pkgs-config/conf-packages.el) 第 14 行：
    
        ;; package repositories
        (setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                                 ("melpa" . "http://mirrors.163.com/elpa/melpa-stable/")))
    
    对于没有下载的 package， [conf-package.el](file:///home/mel/.emacs.d/pkgs-config/conf-packages.el) 文件提供 \`require-package'
    函数用于安装对应 package。

2.  pkgs-config

    pkgs 文件夹内各 pkg 对应的配置文件
    
    当前 pkgs-config 文件夹内配置文件名前缀为 \`conf-‘，后缀为 \`.el'，
    每个配置文件会通过 \`require-package' 函数安装对应文件。
    
    需要注意的是，pkgs 文件夹中 elisp 包的配置除 use-package 包未使用
    \`use-package' 进行启动与配置管理，其他所有 pkgs 文件夹中的 elisp
    包都使用 use-package 进行启动与配置管理。\`use-package' 的使用可以
    参考作者 [github](https://github.com/jwiegley/use-package) 中使用说明。
    
    1.  [conf-preference](file:///home/mel/.emacs.d/pkgs-config/conf-preference.el)
    
        emacs 基础配置
        
        backup 文件夹配置，参考 [conf-preference](file:///home/mel/.emacs.d/pkgs-config/conf-preference.el) 中 31 行。
        
            ;; setup backup dir
            (let* ((emacs-backup-dir (expand-file-name "backup" user-emacs-directory)))
              (unless (file-exists-p emacs-backup-dir)
                (make-directory emacs-backup-dir))
              (setq backup-directory-alist `((".*" . ,(file-name-as-directory emacs-backup-dir)))
                    backup-by-copying t ;; don't delink hardlinks
                    version-control t ;; use version numbers on backups
                    delete-old-versions t ;; automatically delete excess backups
                    kept-new-versions 100 ;; how many of the newest version to keep
                    kept-old-versions 3 ;; how many of the old
                    )) ;; end of backup dir setup
        
        auto-save 文件夹配置，参考 [conf-preference](file:///home/mel/.emacs.d/pkgs-config/conf-preference.el) 中 42 行。
        auto-save-list 文件也会被存在 auto-save 文件夹中
        
            ;; auto-save dir
            (let* ((emacs-auto-save-dir (expand-file-name "auto-save" user-emacs-directory)))
              (unless (file-exists-p emacs-auto-save-dir)
                (make-directory emacs-auto-save-dir))
              (setq auto-save-file-name-transforms
                    `((".*" ,(file-name-as-directory emacs-auto-save-dir) t)))
              (setq auto-save-list-file-prefix
                    (file-name-as-directory emacs-auto-save-dir)))
            
            (provide 'conf-preference)
        
        customize 选项被放在 [.custom.el](file:///home/mel/.emacs.d/.custom.el) 文件中

3.  backup

    backup 目录

4.  auto-save

    auto-save 目录

5.  snippets

    这个文件夹由 \`yasnippet' 创建，可以考虑以后将 snippets 文件夹放到
    安装包配置文件夹件中统一管理，当前不对此文件夹进行处理。可以参考
    \`no-littering' 对目录的管理方式。

6.  [init](file:///home/mel/.emacs.d/init.el)

    启动配置文件
    
        
        (add-to-list 'load-path (expand-file-name "pkgs-config" user-emacs-directory))
        
        (require 'conf-preference)
        
        (require 'conf-packages)
        
        (require 'conf-use-package)
        
        (require 'conf-smex)
        
        (require 'conf-ace-jump-mode)
        
        (require 'conf-undo-tree)
        
        (require 'conf-ggtags)
        
        (require 'conf-elpy)

7.  [readme](file:///home/mel/.emacs.d/readme.md)

    emacs 配置说明，当前 github 上存储的说明文档为最小文档，需要在有
    网络的情况下将配置中的文件下载到本地。

