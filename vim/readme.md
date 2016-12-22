Setup VIM
========================
This setup is intended to generate a very lightweight VIM with a very minimalist plugin. You can see list of plugins on `.vimrc` file.  

Install Ag
------------------------
This use ack.vim with ag as recursive searching tool. Therefore, you need to install the binary first. You can read the instalation manual [here](https://github.com/ggreer/the_silver_searcher)  

Install FZF
------------------------
This use FZF as fuzzy file finder tool. Again, it is required to install the binary first. You can install it by running the command below or read the full documentation [here](https://github.com/junegunn/fzf).  
```bash
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

Install Vundle.vim and Setup VIM Configuration
------------------------
Install Vundle by running this command:  
```bash
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```
Place the `.vimrc` in your home directory, then launch VIM. Run `:PluginInstall` inside it.  

Your VIM is ready to use :rocket:.  
