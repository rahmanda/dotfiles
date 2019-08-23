Setup VIM
========================
This setup is intended to generate a very lightweight VIM with a very minimalist plugin. You can see list of plugins on `.vimrc` file.  

Install Ag
------------------------
This setup use [Ag](https://github.com/ggreer/the_silver_searcher) as recursive searching tool. If you are using Ubuntu, you can install it by running this command:

``` bash
sudo apt-get install silversearcher-ag
```

If you are using other OS, read [this guideline](https://github.com/ggreer/the_silver_searcher#installing).

Install FZF
------------------------
This setup use [FZF](https://github.com/junegunn/fzf) as a fuzzy file finder. Again, it is required to install the binary first. You can install it by running the command below:  

```bash
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
```

Then, add below lines to your `~/.bashrc` file:

```bash
# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -g ""'

# To apply the command to CTRL-T as well
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
```

Don't forget to source your `~/.bashrc` file by running `source ~/.bashrc` command on your terminal.

Install Vundle.vim and Setup VIM Configuration
------------------------
Install Vundle by running this command:  
```bash
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```
Place the `.vimrc` in your home directory, then launch VIM. Then, type `:PluginInstall` and enter. This command will run installation for the plugins.

After `:PluginInstall` process has finished, restart your VIM.

Now your VIM is ready to use :rocket:.  
