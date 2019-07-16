* pyim-wbdict README                         :README:doc:

** 简介
pyim-wbdict 是 pyim 的一个五笔词库包。
1. pyim-wbdict-v98.pyim 源于 emacs-eim 的五笔词库。

** 安装和使用
1. 配置melpa源，参考：http://melpa.org/#/getting-started
2. M-x package-install RET pyim-wbdict RET
3. 在emacs配置文件中（比如: ~/.emacs）添加如下代码：
   #+BEGIN_EXAMPLE
   (require 'pyim-wbdict)
   (pyim-wbdict-v98-enable)
   #+END_EXAMPLE
