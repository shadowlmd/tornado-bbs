{ shared code between NTVDMCLP and WIN32 targets }

function L0(Value: Integer): String;
var
  S: String;
begin
  Str(Value, S);
  if Length(S) = 1 then
    S := '0' + S;
  L0 := S;
end;

function GetSystemTZUTC: String;
var
  TimeZoneInfo: TTimeZoneInformation;
  BiasMinutes, Hours, Minutes: Integer;
  Sign: String[1];
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_UNKNOWN:
      BiasMinutes := TimeZoneInfo.Bias;
    TIME_ZONE_ID_STANDARD:
      BiasMinutes := TimeZoneInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT:
      BiasMinutes := TimeZoneInfo.DaylightBias;
  end;

  Hours := Abs(BiasMinutes) div 60;
  Minutes := Abs(BiasMinutes) mod 60;

  Sign := '';
  if BiasMinutes > 0 then
    Sign := '-';

  GetSystemTZUTC := Sign + L0(Hours) + L0(Minutes);
end;
