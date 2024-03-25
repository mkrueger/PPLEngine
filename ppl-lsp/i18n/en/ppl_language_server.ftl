hint-type-boolean=
    BOOLEAN
    
    unsigned character (1 byte) 0 = FALSE, non-0 = TRUE
hint-type-date=
    DATE
    
    unsigned integer (2 bytes) PCBoard julian date (count of days since 1/1/1900) 
hint-type-ddate=
    DDATE

    Signed long integer for julian date. DDATE is for use with DBase date fields.
    It holds a long integer for julian dates. When coerced to string type it is in the format CCYYMMDD or 19940527
hint-type-integer=
    INTEGER / SDWORD / LONG

    signed long integer (4 bytes) Range: -2,147,483,648 → +2,147,483,647
hint-type-money=
    MONEY
    
    signed long integer (4 bytes) Range: -$21,474,836.48 → +$21,474,836.47
hint-type-string=
    STRING
    
    far character pointer (4 bytes) NULL is an empty string non-NULL points to a string of some length less than or equal to 256
hint-type-time=
    TIME
    
    signed long integer (4 bytes) Count of seconds since midnight
hint-type-bigstr=
    BIGSTR
    
    Allows up to 2048 characters per big string (up from 256 for STRING variables) May include CHR(0) characters in the middle of the big string (unlike STRING variables which may not)
hint-type-edate=
    EDATE
    
    Julian date in earth date format Deals with dates formatted YYMM.DD Range: Same as DATE
hint-type-float=
    REAL / FLOAT

    4-byte floating point number Range: +/-3.4E-38 - +/-3.4E+38 (7-digit precision)
hint-type-double=
    DREAL / DOUBLE

    8-byte floating point number Range: +/-1.7E-308 - +/-1.7E+308 (15-digit precision)
hint-type-unsigned=
    UNSIGNED / DWORD / UDWORD

    4-byte unsigned integer Range: 0 - 4,294,967,295
hint-type-byte=
    BYTE / UBYTE

    1-byte unsigned integer Range: 0 - 255
hint-type-word=
    WORD / UWORD

    2-byte unsigned integer Range: 0 - 65,535
hint-type-sbyte=
    SBYTE / SHORT

    1-byte signed Integer Range: -128 - 127
hint-type-sword=
    SWORD / INT

    2-byte signed integer Range: -32,768 - 32,767

hint-func-end=todo
hint-func-cls=todo
hint-func-clreol=todo
hint-func-more=todo
hint-func-wait=todo
hint-func-color=todo
hint-func-goto=todo
hint-func-let=todo
hint-func-print=
    ### Function
    Print a line to the screen
    ### Syntax
    ```PRINT exp[, exp]*```
    ### Remarks
    This statement will process all @ codes and display them as expected.
hint-func-println=
    ### Function
    Print a line to the screen and append a newline to the end of the expression(s).
    ### Syntax
    ```PRINTLN [exp[, exp]*]?```
    ### Remarks
    This statement will process all @ codes and display them as expected.
hint-func-confflag=todo
hint-func-confunflag=todo
hint-func-dispfile=todo
hint-func-input=todo
hint-func-fcreate=todo
hint-func-fopen=todo
hint-func-fappend=todo
hint-func-fclose=todo
hint-func-fget=todo
hint-func-fput=todo
hint-func-fputln=todo
hint-func-resetdisp=todo
hint-func-startdisp=todo
hint-func-fputpad=todo
hint-func-hangup=todo
hint-func-getuser=todo
hint-func-putuser=todo
hint-func-defcolor=todo
hint-func-delete=todo
hint-func-deluser=todo
hint-func-adjtime=
    ### Function
    Adjust the users time up or down
    ### Syntax
    ```ADJTIME minutes```
    ```minutes``` An integer expression containing the number of minutes to adjust the time left by. > 0 will add time, < 0 will deduct time.
    ### Remarks
    The added/deducted time is only applied to the curent call.
hint-func-log=todo
hint-func-inputstr=todo
hint-func-inputyn=todo
hint-func-inputmoney=todo
hint-func-inputint=todo
hint-func-inputcc=todo
hint-func-inputdate=todo
hint-func-inputtime=todo
hint-func-gosub=todo
hint-func-return=todo
hint-func-promptstr=todo
hint-func-dtron=todo
hint-func-dtroff=todo
hint-func-cdchkon=todo
hint-func-cdchkoff=todo
hint-func-delay=todo
hint-func-sendmodem=todo
hint-func-inc=todo
hint-func-dec=todo
hint-func-newline=todo
hint-func-newlines=todo
hint-func-tokenize=todo
hint-func-gettoken=todo
hint-func-shell=todo
hint-func-disptext=todo
hint-func-stop=todo
hint-func-inputtext=todo
hint-func-beep=todo
hint-func-push=todo
hint-func-pop=todo
hint-func-kbdstuff=todo
hint-func-call=todo
hint-func-join=todo
hint-func-quest=todo
hint-func-blt=todo
hint-func-dir=todo
hint-func-kbdfile=todo
hint-func-bye=todo
hint-func-goodbye=todo
hint-func-broadcast=todo
hint-func-waitfor=todo
hint-func-kbdchkon=todo
hint-func-kbdchkoff=todo
hint-func-optext=todo
hint-func-dispstr=todo
hint-func-rdunet=todo
hint-func-wrunet=todo
hint-func-dointr=todo
hint-func-varseg=todo
hint-func-varoff=todo
hint-func-pokeb=todo
hint-func-pokew=todo
hint-func-varaddr=todo
hint-func-ansipos=todo
hint-func-backup=todo
hint-func-forward=todo
hint-func-freshline=todo
hint-func-wrusys=todo
hint-func-rdusys=todo
hint-func-newpwd=todo
hint-func-opencap=todo
hint-func-closecap=todo
hint-func-message=todo
hint-func-savescrn=todo
hint-func-restscrn=todo
hint-func-sound=todo
hint-func-chat=todo
hint-func-sprint=todo
hint-func-sprintln=todo
hint-func-mprint=todo
hint-func-mprintln=todo
hint-func-rename=todo
hint-func-frewind=todo
hint-func-pokedw=todo
hint-func-dbglevel=todo
hint-func-showon=todo
hint-func-showoff=todo
hint-func-pageon=todo
hint-func-pageoff=todo
hint-func-fseek=todo
hint-func-fflush=todo
hint-func-fread=todo
hint-func-fwrite=todo
hint-func-fdefin=todo
hint-func-fdefout=todo
hint-func-fdget=todo
hint-func-fdput=todo
hint-func-fdputln=todo
hint-func-fdputpad=todo
hint-func-fdread=todo
hint-func-fdwrite=todo
hint-func-adjbytes=todo
hint-func-kbdstring=todo
hint-func-alias=todo
hint-func-redim=todo
hint-func-append=todo
hint-func-copy=todo
hint-func-kbdflush=todo
hint-func-mdmflush=todo
hint-func-keyflush=todo
hint-func-lastin=todo
hint-func-flag=todo
hint-func-download=todo
hint-func-wrusysdoor=todo
hint-func-getaltuser=todo
hint-func-adjdbytes=todo
hint-func-adjtbytes=todo
hint-func-adjtfiles=todo
hint-func-lang=todo
hint-func-sort=todo
hint-func-mousereg=todo
hint-func-scrfile=todo
hint-func-searchinit=todo
hint-func-searchfind=todo
hint-func-searchstop=todo
hint-func-prfound=todo
hint-func-prfoundln=todo
hint-func-tpaget=todo
hint-func-tpaput=todo
hint-func-tpacget=todo
hint-func-tpacput=todo
hint-func-tparead=todo
hint-func-tpawrite=todo
hint-func-tpacread=todo
hint-func-tpacwrite=todo
hint-func-bitset=todo
hint-func-bitclear=todo
hint-func-brag=todo
hint-func-frealtuser=todo
hint-func-setlmr=todo
hint-func-setenv=todo
hint-func-fcloseall=todo
hint-func-declare=todo
hint-func-function=todo
hint-func-procedure=todo
hint-func-pcall=todo
hint-func-fpclr=todo
hint-func-begin=todo
hint-func-fend=todo
hint-func-static=todo
hint-func-stackabort=todo
hint-func-dcreate=todo
hint-func-dopen=todo
hint-func-dclose=todo
hint-func-dsetalias=todo
hint-func-dpack=todo
hint-func-dcloseall=todo
hint-func-dlock=todo
hint-func-dlockr=todo
hint-func-dlockg=todo
hint-func-dunlock=todo
hint-func-dncreate=todo
hint-func-dnopen=todo
hint-func-dnclose=todo
hint-func-dncloseall=todo
hint-func-dnew=todo
hint-func-dadd=todo
hint-func-dappend=todo
hint-func-dtop=todo
hint-func-dgo=todo
hint-func-dbottom=todo
hint-func-dskip=todo
hint-func-dblank=todo
hint-func-ddelete=todo
hint-func-drecall=todo
hint-func-dtag=todo
hint-func-dseek=todo
hint-func-dfblank=todo
hint-func-dget=todo
hint-func-dput=todo
hint-func-dfcopy=todo
hint-func-eval=todo
hint-func-account=todo
hint-func-recordusage=todo
hint-func-msgtofile=todo
hint-func-qwklimits=todo
hint-func-command=todo
hint-func-uselmrs=todo
hint-func-confinfo=todo
hint-func-adjtubytes=todo
hint-func-grafmode=todo
hint-func-adduser=todo
hint-func-killmsg=todo
hint-func-chdir=todo
hint-func-mkdir=todo
hint-func-redir=todo
hint-func-fdowraka=todo
hint-func-fdoaddaka=todo
hint-func-fdowrorg=todo
hint-func-fdoaddorg=todo
hint-func-fdoqmod=todo
hint-func-fdoqadd=todo
hint-func-fdoqdel=todo
hint-func-sounddelay=todo
hint-func-shortdesc=todo
hint-func-movemsg=todo
hint-func-setbankbal=todo
