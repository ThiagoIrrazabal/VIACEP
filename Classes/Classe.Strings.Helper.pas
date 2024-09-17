unit Classe.Strings.Helper;

interface

uses
  System.RegularExpressions;

type
  TStringHelper = record helper for string
  public
    function ReplaceAccentuationByUnderline: string;
    function ReplaceAccentuation: string;
  end;

implementation

{ TStringHelper }

function TStringHelper.ReplaceAccentuation: string;
var
  lRegex: TRegEx;
begin
  lRegex := TRegEx.Create('[áàâãä]', [roIgnoreCase]);
  Result := lRegex.Replace(Self, 'A');

  lRegex := TRegEx.Create('[éèêë]', [roIgnoreCase]);
  Result := lRegex.Replace(Result, 'E');

  lRegex := TRegEx.Create('[íìîï]', [roIgnoreCase]);
  Result := lRegex.Replace(Result, 'I');

  lRegex := TRegEx.Create('[óòôõö]', [roIgnoreCase]);
  Result := lRegex.Replace(Result, 'O');

  lRegex := TRegEx.Create('[úùûü]', [roIgnoreCase]);
  Result := lRegex.Replace(Result, 'U');

  lRegex := TRegEx.Create('[ç]', [roIgnoreCase]);
  Result := lRegex.Replace(Result, 'C');
end;

function TStringHelper.ReplaceAccentuationByUnderline: string;
var
  lRegEx: TRegEx;
begin
  lRegEx := TRegEx.Create('[ÁÀÂÃÄáàâãäÉÈÊËéèêëÍÌÎÏíìîïÓÒÔÕÖóòôõöÚÙÛÜúùûüÇç]', [roIgnoreCase]);
  Result := lRegEx.Replace(Self, '_');
end;

end.
