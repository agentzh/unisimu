do L:=1
while (L>0) {
    if (L=1) {
        if (p) {
            do L:=2
        } else {
            do L:=3
        }
    } else {
        if (L=2) {
            do f
            do L:=4
        } else {
            if (L=3) {
                do g
                do L:=5
            } else {
                if (L=4) {
                    do h
                    do L:=0
                } else {
                    if (L=5) {
                        if (q) {
                            do L:=4
                        } else {
                            do L:=0
                        }
                    }
                }
            }
        }
    }
}
