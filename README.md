# dotfiles

If you want, after cloning the repo, save your dotfile and link the new one with
```bash
mv ~/.bash_profile ~/.bash_profile.save
mv ~/.bashrc ~/.bashrc.save
mv ~/.gitconfig ~/.gitconfig.save
mv ~/.Rprofile ~/.Rprofile.save
mv ~/.vimrc ~/.vimrc.save
ln -s ~/dotfiles/bash_profile ~/.bash_profile
ln -s ~/dotfiles/bashrc ~/.bashrc
ln -s ~/dotfiles/gitconfig ~/.gitconfig
ln -s ~/dotfiles/liquidpromptrc ~/.liquidpromptrc
ln -s ~/dotfiles/Rprofile ~/.Rprofile
ln -s ~/dotfiles/vimrc ~/.vimrc
```
After every further `git pull`, the dotfile link gets updated automatically.

The `Rprofile` results in (check the `functions` repo for function defs):
<br><br>
<img align="left" width="600" src="screen_rprofile.png">

# todo

- `~/.gitconfig`

