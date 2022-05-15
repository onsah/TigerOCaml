#!/usr/bin/env nu

def getOrError [symbol] {
	let x = ($env | select $symbol);
	if ($x | length) != 0 {
		$env | get $symbol;
	} else {
		echo "$symbol is not defined"; 
		exit 1;
	}
}

let project_home = (getOrError "PROJECT_HOME");
let test_file = (getOrError "TEST_FILE");

cd $project_home; 
dune exec src/TigerC.exe $test_file