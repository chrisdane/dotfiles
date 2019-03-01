# dots

To link these dotfiles to `~/` paste

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

and run

```
$ chmod 755 link_all
$ ./link_all
```

