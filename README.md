# dots

To link simply run

```
#!/bin/bash

fs=(bashrc vimrc Rprofile) # change here

for i in "${fs[@]}"; do
    echo ln -sf $PWD/$i ~/.$i
    ln -sf $PWD/$i ~/.$i
done
```
