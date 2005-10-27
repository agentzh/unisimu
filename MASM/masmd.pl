#: masmd.pl.tt
#: 2005-10-14 2005-10-15

use strict;
use warnings;

my $fname = shift;
die "No MASM file specified.\n" if !defined $fname;
$fname .= '.asm' if $fname !~ m/\.asm$/i;

open my $in, $fname or
    die "error: Can't open '$fname' for reading: $!\n";
open my $fh, 

my ($pcnt, $lcnt, $scnt) = (0, 0, 0);
my $in_codes = 0;

# Add guard procedure to prevent user's procs fall down
# into masmd's routines
my $proc_defs = <<'_EOC_';
_masmd_P proc
    ret
_masmd_P endp
_EOC_
while (<$in>) {
    if (/^\s*;\s*(?:(say|print)(10|16)?)\s*(.*);?\s*$/i) {
        my $old = $_;
        chomp $old;
        $in_codes = 1;
        my $cmd = lc($1);
        my $fmt = $2 || 10;  # default to 10
        my $args = $3;
        my @strings;
        $pcnt++;
        $proc_defs .= <<"_EOC_";

; procedure for ``$old'':
_masmd_P$pcnt proc
_EOC_
        while (1) {
            if ($args =~ m/\G\s*("[^"]*"|'[^']*')\s*,?/gco) {
                #warn "! ($.): $& ", pos($args), "\n";
                my $s = $1;
                my $old = $s;
                $s =~ s/\\n/",0dh,0ah,"/g;
                $s =~ s/\"\",|\"\"$//g;
                #$s .= ",0dh,0ah" if $cmd eq 'say';
                $s .= ",'\$'";
                $s =~ s/,,+/,/g;
                $lcnt++; $scnt++;
                $proc_defs .= <<_EOC_;

    ; output constant string $old:
    jmp _masmd_L$lcnt
    _masmd_S$scnt db $s
_masmd_L$lcnt:
    push dx
    push ax
    push ds
    mov ax, cs
    mov ds, ax
    mov dx, offset _masmd_S$scnt
    mov ah, 09h
    int 21h
    pop ds
    pop ax
    pop dx

_EOC_

            } elsif ($args =~ m/\G\s*([^"',]+)\s*,?/gco) {
                my $var = $1;
                #warn "!! ($.) $& ", pos($args), "\n";
                $proc_defs .= "    ; output variable $var:\n";
                if ($var ne 'ax') {
                    $proc_defs .= <<"_EOC_";
    push ax
    mov ax, $var
_EOC_
                }
                $proc_defs .= "    call _masmd_outw$fmt\n";
                if ($var ne 'ax') {
                    $proc_defs .= "    pop ax\n";
                }
            }elsif ($args =~ m/\G.*\S/gco) {
                die "Syntax error: line $.: character '$& ' unexpected.\n";
            } else {
                #warn "!!! ($.) ", pos($args), "\n";
                last;
            }
        }
        if ($cmd eq 'say') {
            $proc_defs .= <<'_EOC_';

    ; output the trailing "\n" for ``say'':
    push dx
    push ax
    mov ah, 02h
    mov dl, 0dh
    int 21h
    mov dl, 0ah
    int 21h
    pop ax
    pop dx

_EOC_
        }
        $proc_defs .= "ret\n_masmd_P$pcnt endp\n";
        print;
        print "    call _masmd_P$pcnt\n";
    } elsif ($in_codes and m/^\s*\w+\s+ends\b/i) {
        $proc_defs .= <<'_EOC_';

_masmd_outw10 proc
    push ax
    push bx
    push cx
    push dx

    ; say "Entering outw10 with ax = ", ax
    test ax, ax
    ;; say "Here?"
    jns _masmd_outw10_1
    neg ax

    ; say "ax is negative"
    push ax
    mov dl, '-'
    mov ah, 02h
    int 21h
    pop ax

_masmd_outw10_1:
    ;; say "Here??"
    xor cx, cx
    mov bx, 10
_masmd_outw10_2:
    xor dx,dx
    div bx
    ;; say "Here???"
    
    ;; say "Producing digit ", dx, "..."
    push dx  ; save the decimal digit one by one
    inc cx
    
    test ax, ax
    jnz _masmd_outw10_2

_masmd_outw10_3:
    pop dx   ; restore the decimal digit one by one
    
    ;; say "Outputing digit ", dx, "..."
    add dl, 30h
    mov ah, 02h
    int 21h

    loop _masmd_outw10_3
    
    pop dx
    pop cx
    pop bx
    pop ax

    ret
_masmd_outw10 endp


_masmd_outw16 proc
    push ax
    push cx
    push dx
    push bx

    mov bx, ax
    mov cl, 16-4

_masmd_outw16_1:
    mov ax, bx
    shr ax, cl

    mov dl, al
    and dl, 0fh

    cmp dl, 9
    jg _masmd_outw16_2

    add dl, 30h
    jmp _masmd_outw16_3

_masmd_outw16_2:
    add dl, 41h-0ah

_masmd_outw16_3:
    mov ah, 02h
    int 21h

    sub cl, 4
    jge _masmd_outw16_1

    pop bx
    pop dx
    pop cx
    pop ax
    ret
_masmd_outw16 endp

_EOC_
        print "$proc_defs\n";
        print;
    } else {
        print;
    }
}

close $in;
