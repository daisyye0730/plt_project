import glob
import subprocess
import itertools
import build

# LLI location
LLI = "lli"

# test all files matching a given pattern - e.g. "tests/test-*.dino"
def test(files):
    passes = 0
    num_tests = len(files)
    test_no = 1
    
    for file in files:
        passed = True
        ll_file = file[0:file.index(".dino")] + ".ll"
        err_file = file[0:file.index(".dino")] + ".err"
        gold_standard = file[0:file.index(".dino")] + ".out"
        print("(" + str(test_no) + "/" + str(num_tests) + ") Running " + file)
        
        # attempt to generate the LLVM code resulting from this program
        result = subprocess.run(['./dinosaur.native', file],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output = ""
        
        # if LLVM code generates successfully, run it and compare the output to gold standard.
        if result.returncode == 0:
            llvm = result.stdout.decode('utf-8')
            ll = open(ll_file, "w")
            ll.write(llvm)
            ll.close()
            
            compiles = subprocess.run(
                [LLI, ll_file], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            # get the output of running the LLVM code
            if compiles.returncode == 0:
                output = compiles.stdout.decode('utf-8').splitlines(True)
            # cleanup
            subprocess.run(["rm", ll_file])
        else:
			# get the error from trying to compile the LLVM code
            output = result.stderr.decode('utf-8').splitlines(True)

		# compare our output with the gold standard for this test
        with open(gold_standard) as gs:
            for line1, line2 in itertools.zip_longest(output, gs, fillvalue=""):
                if line1 != line2:
                    print("DIFF: " + line1 + " vs. " + line2)
                    passed = False

        if passed:
            print("+ SUCCESS")
            passes += 1
        else:
            print("- FAILED")
        
        test_no += 1
    return passes


# run passing test cases
def test_pass():
	print("\n")
	print("*** POSITIVE TEST CASES ***")
	print("(These should SUCCEED)")

	files = glob.glob("tests/test-*.dino")
	
	if len(files) == test(files):
		return True

# run failing test cases
def test_fail():
	print("\n")
	print("*** NEGATIVE TEST CASES ***")
	print("(These should SUCCEED)")

	files = glob.glob("tests/fail-*.dino")
	
	if len(files) == test(files):
		return True

# MAIN
# Check that all test cases pass.
if test_pass() and test_fail():
	print("\nTEST SUITE PASSED\n")
else:
	print("\nTEST SUITE FAILED\n")
