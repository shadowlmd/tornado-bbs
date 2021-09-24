{*********************************************************}
{*                  APDEFINE.INC 2.03                    *}
{*      Assorted conditional compilation directives      *}
{*        Copyright (c) TurboPower Software 1991         *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$IFDEF Windows}
  !! ERROR: Async Professional is not compatible with Windows !!
{$ENDIF}

{Enable one or more of the following defines. These specify which "device
layer" will be included in your Async Professional programs.}

{$DEFINE UseUart}
{$DEFINE UseInt14}
{$DEFINE UseFossil}
{$DEFINE UseDigi14}

{Valid Alternatives:
  UseUart
  UseInt14
  UseFossil
  UseDigi14}

{Enable _only_ one of the following UART control options. This will
determine what options are included in the interrupt handler. Enabling more
features means that you will have a lower top speed. See the manual for more
details and some guidelines for choosing options. In most cases, the Standard
option should be used.}

{$DEFINE Standard}

{Valid Alternatives:
  Standard        - Automatic software and hardware flow control
  StandardLogging - Standard plus EventLogging
  Status          - Standard plus StatusBuffering
  StatusLogging   - Status plus EventLogging
  HWOnly          - Automatic hardware flow control only
  SWOnly          - Automatic software flow control only
  Basic           - No options at all (no flow, no status, no logging)
  UserDefined     - User-customized options}

{Enable this define if you are using the OOP calling format for Async
Professional. This doesn't affect the operation of Async Professional at all
but will save a few hundred bytes of code space.}

{.$DEFINE UseOOP}

{This define enables the application-level tracing of transmitted and received
characters. See the manual for more information on Tracing. Since Tracing is
considered a debugging facility, it's not normally defined.}

{.$DEFINE Tracing}

{Enable this define if you are using Object Professional with Async
Professional. This will allow APro to use many of OPro's low-level routines
from OPROOT, OPSTRING, OPINLINE, and OPDATE.}

{.$DEFINE UseOPro}

{Enable this define if you are using Turbo Professional with Async
Professional. This will allow APro to use many of TPro's low-level routines
from TPMEMCHK, TPSTRING, TPINLINE, and TPDATE.}

{.$DEFINE UseTPro}

{This define controls whether the device layer (apuart or apint14) is
"registered" automatically. Under rare cirumstances, simple communications
applications may get smaller (by 1 or 2K) if you turn this option off and
manually "register" just those device layer procedures you're using. See the
manual (on device layers) before turning this option off}

{$DEFINE AutoDeviceInit}

{This define controls how many ComNames are defined. By default, APro defines
only eight ComNames (Com1..Com8). This is all our devices layers need or
will use. However, device layers you create may need more. If so, use this
define to create Com1..Com36. Note, however, that our device layers will
continue to use only Com1..Com8.}

{$DEFINE LargeComNameSet}

{This define controls whether the procedural portion of Async Professional
is built as TPU/TPP files or as two protected mode DLLs (APCOMD for the
device and interface layers and APPROTD for all protocols).}

{.$DEFINE UsePmodeDLL}

{This define controls whether APFAX.FNT, the font file used to
convert ASCII text files to fax binary files, is bound to the
APFAXCVT/OOFAXCVT units or not. Binding the font to the TPU results in
fast loads from LoadFont but adds about 16K of code.}

{.$DEFINE BindFaxFont}

{-------------------- Internal Use Only -----------------------------}

{.$DEFINE LeakCheck}

{----------------------  Error checks  ------------------------------}

{$IFDEF UseOPro}
{$IFDEF UseTPro}
  !! STOP COMPILE - UseOPro and UseTPro cannot both be defined
{$ENDIF}
{$ENDIF}

{$IFDEF UsePmodeDLL}
{$IFDEF UseOPro}
  !! STOP COMPILE - UsePmodeDLL is not intended to be used with UseOPro
{$ENDIF}
{$ENDIF}

{$IFDEF UsePmodeDLL}
{$IFDEF UseTPro}
  !! STOP COMPILE - UsePmodeDLL is not intended to be used with UseTPro
{$ENDIF}
{$ENDIF}

{$IFDEF UsePmodeDLL}
{$IFNDEF DPMI}
  !! STOP COMPILE - UsePmodeDLL can be used only in protected mode
{$ENDIF}
{$ENDIF}

{-------- !! DON'T CHANGE DEFINES BELOW THIS LINE !! -----------------}

{Set EventLogging based on selected UART configuration}
{$IFDEF UseUart}
{$IFDEF StandardLogging}
  {$DEFINE EventLogging}
{$ENDIF}
{$IFDEF StatusLogging}
  {$DEFINE EventLogging}
{$ENDIF}
{$ENDIF}

{Set UseSWFlow based on selected UART configuration}
{$IFDEF Standard}
  {$DEFINE UseSWFlow}
{$ENDIF}
{$IFDEF StandardLogging}
  {$DEFINE UseSWFlow}
{$ENDIF}
{$IFDEF SWOnly}
  {$DEFINE UseSWFlow}
{$ENDIF}
{$IFDEF Status}
  {$DEFINE UseSWFlow}
{$ENDIF}
{$IFDEF StatusLogging}
  {$DEFINE UseSWFlow}
{$ENDIF}

{Set UseHWFlow based on selected UART configuration}
{$IFDEF Standard}
  {$DEFINE UseHWFlow}
{$ENDIF}
{$IFDEF StandardLogging}
  {$DEFINE UseHWFlow}
{$ENDIF}
{$IFDEF HWOnly}
  {$DEFINE UseHWFlow}
{$ENDIF}
{$IFDEF Status}
  {$DEFINE UseHWFlow}
{$ENDIF}
{$IFDEF StatusLogging}
  {$DEFINE UseHWFlow}
{$ENDIF}

{Set StatusBuffering based on selected UART configuration}
{$IFDEF Status}
  {$DEFINE StatusBuffering}
{$ENDIF}
{$IFDEF StatusLogging}
  {$DEFINE StatusBuffering}
{$ENDIF}

{Set UserDefined as desired (currently = UseHWFlow + EventLogging)}
{$IFDEF UserDefined}
  {$DEFINE UseHWFlow}
  {$DEFINE EventLogging}
{$ENDIF}

{Set Standalone based on UseOPro and UseTPro values}
{$IFNDEF UseOPro}
  {$IFNDEF UseTPro}
    {$DEFINE Standalone}
  {$ENDIF}
{$ENDIF}

{The following define tells the APro heap error function how to handle heap
errors.}

{$IFDEF Ver60}
  {$DEFINE Heap6}
{$ENDIF}

{$IFDEF Ver70}
  {$DEFINE Heap6}
  {$Q-,T-,X-}
{$ENDIF}
