while (<>) {
  s/Date: [^<]+<br/<br/;
  s/Date:[^<]+</</;
  s/SAXON HE 9.*//;
  s/on 20[0-9][0-9].[0-9][0-9].[0-9][0-9]T.*Z\.?//;
  print;
}
