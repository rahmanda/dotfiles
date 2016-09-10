Setup Emacs
============================
This assumes you have a clean emacs ver. >= 24 installation and python ver. >= 2.6.

Install Cask
----------------------------
Clone cask from github:  
```bash
git clone https://github.com/cask/cask.git ~/.cask
```  
and add this `export PATH="$PATH:$HOME/.cask/bin"` to `~/.profile`.  
Next, copy all code from this repository's `init.el` file into `~/.emacs.d/init.el` (if init file is not exist, just create it beforehand).  

Install Pallet
----------------------------
Copy all code from this repository's Cask file into `~/.emacs.d/Cask`.  

Restart your terminal, go to `~/.emacs.d/` and run:  
```bash
cask install
```  
It will create `~/.emacs.d/.cask` folder to store your packages.  

You can install whatever additional packages you want either via `package-list-packages` or `package-install` directly from emacs without hassle. Cask and Pallet will manage your package dependencies. Don't forget to backup your `~/.emacs.d/init.el` and `~/.emacs.d/Cask` files every time you update emacs packages.  
