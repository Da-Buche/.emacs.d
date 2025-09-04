# .emacs.d - Personal Emacs Ninja Setup 🥷
Author: Aurélien Buchet

## Setup

Create `~/.emcas.d` to load `~/.emacs.d/load.el`.
```sh
test ! -d ~/.emacs.d && (cd ~ && git clone https://github.com/Da-Buche/.emacs.d .emacs.d && ln -s .emacs.d/.emacs . ;) || (>&2 echo "~/.emacs.d exists already." ; exit 1 ;)
```

## References & Thanks

http://github.com/ananthakumaran/typescript.el  
https://github.com/yoshiki/yaml-mode  
https://github.com/lukhas/buffer-move  
http://github.com/nonsequitur/smex  

## License

Licensed under GPL-3.0 license as most projects mentionned above are using it.

