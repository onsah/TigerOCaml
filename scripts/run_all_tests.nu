let testcases_path = "/home/zer0/Projects/compilerOCaml/Files/tiger/testcases";
ls $testcases_path | each { $it.name } | each { dune exec ./TigerC.exe $it }