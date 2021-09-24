{ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป}
{บ      -- THIS FILE IS PART OF THE LIVESYSTEMS COMPRESSOR TOOLBOX. --      บ}
{บ          ALL RIGHTS RESERVED  (C) COPYRIGHTED G. HOOGTERP 1994           บ}
{บ                                                                          บ}
{บ             See the documentation for details on the license.            บ}
{บ                                                                          บ}
{ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
;
; This piece of code is written by Jan Derk Otter. With special thanks!
;

                .model  tpascal
                .code

                proc    BasicCompressorObject@Searchbuffer
                public  BasicCompressorObject@Searchbuffer

                arg     Buf:dword, Zise:word, Start:word, Stop:word, String:dword, Found:Dword

                push    ds
                push    es

                les     di, [Buf]
                add     di, [Start]
                mov     cx, [Stop]
                sub     cx, [Start]
                cld

Start_Search:
                lds     si, [String]
                mov     al, [ds:si+1]
                repne   scasb
                jne     EOS_And_Not_Found

                mov     bx, di
                mov     dx, cx
                mov     cl, [ds:si]
                dec     cl
                mov     ch, 0
                add     si, 2
                repe    cmpsb
                je      EOS_And_Found

                mov     di, bx
                mov     cx, dx
                jmp     Start_Search

EOS_And_Found:
                lds     si, [String]
                mov     al, [ds:si]
                mov     ah, 0
                cmp     ax, dx
                jae     EOS_And_Not_Found

                sub     bx, word ptr [Buf]
                dec     bx
                les     di, [Found]
                mov     word ptr [es:di+2], 0
                mov     word ptr [es:di  ], bx
                mov     al, 1
                pop     es
                pop     ds
                ret

EOS_And_Not_Found:
                mov     bx, [Stop]
                les     di, [Found]
                mov     word ptr [es:di+2], 0
                mov     word ptr [es:di  ], bx
                mov     al, 0
                pop     es
                pop     ds
                ret
                endp    BasicCompressorObject@SearchBuffer

                end

