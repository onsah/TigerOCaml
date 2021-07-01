let testcases_path = "./Files/tiger/testcases";
ls $testcases_path | each { $it.name } | each { echo $"($it): (char newline)";  dune exec ./TigerC.exe $it  }