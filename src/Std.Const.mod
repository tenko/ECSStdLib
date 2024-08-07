(** Module with common library constants *)
MODULE Const IN Std;

CONST
    (* Platform *)
    SysNone*    = 0;
    SysWindows* = 1;
    SysLinux*   = 2;
    (* Log *)
    DEBUG*  = 4;
    INFO*   = 3;
    WARN*   = 2;
    ERROR*  = 1;
    FATAL*  = 0;
    (* Formatting *)
    Left* = {0};     Right* = {1};    Center* = {2};
    Sign* = {3};     Zero* = {4};     Spc* = {5};
    Alt* = {6};      Upper* = {7};
    Fix* = {8};      Exp* = {9};
    (* LongReal / Real *)
    FPZero* = 0;
    FPNormal* = 1;
    FPSubnormal* = 2;
    FPInfinite* = 3;
    FPNaN* = 4;
    (* Stream Open flags *)
    AccessRead*   = {0};
    AccessWrite*  = {1};
    ModeNew*      = {2}; (* forces file to be created or truncated  *)
    (* Stream Seek *)
    SeekSet* = 0;
    SeekCur* = 1;
    SeekEnd* = 2;
    (* Stream Error *)
    OK* = 0;
    ErrorNotImplemented* = 1;
    ErrorAlreadyExists* = 2;
    ErrorPathNotFound* = 3;
    ErrorFileNotFound* = 4;
    ErrorDirNotEmpty* = 5;
    ErrorNoAccess* = 6;
    ErrorLocked* = 7;
    ErrorIncompleteRead* = 8;
    ErrorWriteFailed* = 9;
    ErrorNameToLong* = 10;
    ErrorAlreadyOpen* = 11;
    ErrorUnknown* = 999;
END Const.