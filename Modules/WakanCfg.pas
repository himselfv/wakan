unit WakanCfg;

interface

procedure LoadWakanCfg(const filename: string);

implementation
uses SysUtils, Classes, JWBStrings,
  JWBDic,
  JWBDicSearch,
  JWBCharData,
  JWBSettings,
  JWBRomaSort,
  JWBUnit;

procedure LoadWakanCfg(const filename: string);
var sl: TStringList;
  i: integer;
  ln: string;
  sect:integer;
begin
  defll.Clear;
  suffixl.Clear;
  partl.Clear;
  ClearRomaSortRecords;

  sl := TStringList.Create();
  try
    sl.LoadFromFile(filename);
    sect:=0;
    for i := 0 to sl.Count - 1 do begin
      ln := sl[i];
      if (length(ln)>0) and (ln[1]<>';') then
      begin
        if ln[1]='['then
        begin
          delete(ln,length(ln),1);
          delete(ln,1,1);
          if ln='Particles'then sect:=1 else
          if ln='Deflection'then sect:=2 else
          if ln='CharInfo'then sect:=5 else
          if ln='RomajiSort'then sect:=6 else
          if ln='Suffixes'then sect:=7 else
          if ln='IgnoreWords'then sect:=8 else
          sect:=0;
        end else
         //Some of the fields are in hex unicode, so we have to convert them
          case sect of
            1: partl.Add(ln);
            2: defll.Add(ln);
            5: AddCharPropType(ln);
            6: AddRomaSortRecord(ln);
            7: suffixl.Add(copy(ln,1,1)+autohextofstr(copy(ln,2,Length(ln)-1))); //Format: {type:char}{suffix:fhex}
            8: ignorel.Add(fstr(ln));
          end;
      end;
    end;

   // Load roma_db; these files must be present
    roma_db.Clear;
    roma_db.LoadFromFile(AppFolder+'\'+KunreishikiRoma);
    roma_db.LoadFromFile(AppFolder+'\'+HepburnRoma);
   // roma_user was already read from settings

    rpy_db.Clear;
    rpy_db.LoadFromFile(AppFolder+'\'+PinYinRoma);
   // rpy_user was read from settings

  finally
    FreeAndNil(sl);
  end;

  suffixl.Sorted:=true;
  suffixl.Sort;
end;

end.
