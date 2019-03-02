# dotfiles

To link these dotfiles to your `~/`, copy-paste

```
$ cat > link_all <<EOF
#!/bin/bash

fs=(vimrc Rprofile) 

for i in "${fs[@]}"; do
    echo ln -sf $PWD/$i ~/.$i
    ln -sf $PWD/$i ~/.$i
done
EOF
```

and then run

```
$ chmod 755 link_all
$ ./link_all
```

The `Rprofile` results in:
<br><br>
<img align="left" width="500" src="screen_rprofile.png">

