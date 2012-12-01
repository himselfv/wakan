unit LangExpMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  czs,ens:TStringList;

implementation

{$R *.DFM}

procedure wstr(var t:textfile;s:string;n:integer);
var s2:string;
begin
  if s='' then showmessage(inttostr(n));
  while length(s)>0 do
  begin
    if pos('\',s)>0 then
    begin
      s2:=copy(s,1,pos('\',s)-1);
      delete(s,1,pos('\',s));
    end else
    begin
      s2:=s;
      s:='';
    end;
    if s<>'' then writeln(t,Format('%5.5d',[n])+'+'+s2) else
      writeln(t,Format('%5.5d',[n])+'>'+s2);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var sr:TSearchRec;
    ent,czt,int,cor:textfile;
    s,s2,st,stc,sten,stcz:string;
    i:integer;
    num:integer;
    cont:boolean;
    newrec:boolean;
    recno:integer;
begin
  assignfile(ent,edit1.text);
  assignfile(czt,edit2.text);
  append(ent);
  append(czt);
  czs:=TStringList.Create;
  ens:=TStringList.Create;
  writeln(ent,'#VERS>'+edit3.text);
  writeln(czt,'#VERS>'+edit3.text);
  num:=strtoint(edit4.text);
  if FindFirst('*.*',faAnyFile,sr)=0 then
  repeat
    if (pos('.PAS',uppercase(sr.name))>0) or (pos('.DFM',uppercase(sr.name))>0) then
    begin
      assignfile(int,sr.name);
      reset(int);
      assignfile(cor,'corsource\'+sr.name);
      rewrite(cor);
      newrec:=false;
      cont:=false;
      st:='';
      while not eof(int) do
      begin
        readln(int,s2);
        i:=1;
        s:='';
        while i<=length(s2) do
          if (i<length(s2)) and (s2[i]+s2[i+1]='''''') then
          begin
            inc(i,2); s:=s+'''';
          end else begin s:=s+s2[i]; inc(i); end;
        s2:='';
        while pos('''',s)>0 do
        begin
          s2:=s2+copy(s,1,pos('''',s)-1)+'''';
          delete(s,1,pos('''',s));
          if pos('''',s)=0 then showmessage('Ap. error in '+sr.name+' - '+s);
          stc:=copy(s,1,pos('''',s)-1);
          if stc[1]='#'then
          begin
            s2:=s2+stc+'''';
            delete(s,1,pos('''',s));
          end else
          begin
            if not cont then newrec:=false;
            if (length(stc)>0) and (stc[1]='^') and (length(stc)>6) and (not cont) then
            begin
              recno:=ens.IndexOf(stc);
              newrec:=false;
              if recno=-1 then
              begin
                ens.Add(stc);
                recno:=ens.Count-1;
                newrec:=true;
              end;
              num:=recno+strtoint(edit4.text);
              s2:=s2+'#'+format('%5.5d',[num])+stc+''''
            end else s2:=s2+stc+'''';
            delete(s,1,pos('''',s));
            cont:=false;
            while (length(s)>0) and (s[1]=' ') do delete(s,1,1);
            if (length(s)>0) then
              case s[1] of
                '+':cont:=true;
                '#':begin
                      if (s[2]='3') and (s[3]='9') then stc:=stc+'~'else stc:=stc+'\';
                      cont:=true;
                    end;
              end;
            st:=st+stc;
            if not cont then
            begin
              if newrec then
              begin
                sten:=st;
                stcz:=st;
                if pos('^e',sten)>0 then delete(sten,1,pos('^e',sten)+1) else sten:='';
                if pos('^c',stcz)>0 then delete(stcz,1,pos('^c',stcz)+1) else stcz:='';
                if pos('^',stcz)>0 then stcz:=copy(stcz,1,pos('^',stcz)-1);
                if pos('^',sten)>0 then sten:=copy(sten,1,pos('^',sten)-1);
                while pos('''',sten)>0 do sten[pos('''',sten)]:='''';
                while pos('''',stcz)>0 do stcz[pos('''',stcz)]:='''';
                if sten='' then showmessage(sr.name+'en'+st);
                if stcz='' then showmessage(sr.name+'cz'+st);
                if newrec then wstr(ent,sten,num);
                if newrec then wstr(czt,stcz,num);
              end else if newrec then showmessage('Nonpass - '+st);
              st:='';
              newrec:=false;
            end;
          end;
        end;
        s2:=s2+s;
        while pos('''',s2)>0 do s2:=copy(s2,1,pos('''',s2)-1)+''''''+copy(s2,pos('''',s2)+1,length(s2)-pos('''',s2));
        writeln(cor,s2);
      end;
      if newrec then showmessage('!!!');
      closefile(int);
      closefile(cor);
    end;
  until FindNext(sr)<>0;
  FindClose(sr);
  closefile(czt);
  closefile(ent);
  showmessage('Done');
end;

end.
