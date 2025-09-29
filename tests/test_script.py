import os
import subprocess
import argparse

# Parse command-line arguments
parser = argparse.ArgumentParser(description="Run tests for llracket.")
parser.add_argument("--llracket", required=True, help="Path to the llracket binary.")
parser.add_argument("--runtime", required=True, help="Path to the runtime.c file.")
parser.add_argument("--build-dir", required=True, help="Path to the build directory.")
args = parser.parse_args()


def run_test(
    test_program, test_input, test_output, expect_error, build_dir, quiet=False
):
    """
    Runs the multi-step test process for a given .rkt file.
    """
    dir_name = os.path.basename(os.path.dirname(test_program))
    file_name = os.path.splitext(os.path.basename(test_program))[0]
    test_name = f"{dir_name}_{file_name}"
    print(f"Running test: {test_name}")

    # Define paths for intermediate and output files in the build directory
    ll_file = os.path.join(build_dir, f"{test_name}.ll")
    s_file = os.path.join(build_dir, f"{test_name}.s")
    executable = os.path.join(build_dir, test_name)

    # Step 1: Run llracket to generate the .ll file
    result = subprocess.run(
        [args.llracket, test_program, "-o", ll_file],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        if expect_error:
            if not quiet:
                print(f"✅ Test passed (expected error): {test_name}")
            return True
        print(f"❌ Step 1 failed (llracket): {test_name}")
        print(result.stderr)
        return False
    else:
        if expect_error:
            print(f"❌ Test failed (unexpected success): {test_name}")
            print(result.stdout)
            return False

    # Step 2: Run llc to generate the .s file
    result = subprocess.run(
        ["llc", "-relocation-model=pic", "-o", s_file, ll_file],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"❌ Step 2 failed (llc): {test_name}")
        print(result.stderr)
        return False

    # Step 3: Run clang to generate the executable
    result = subprocess.run(
        ["clang", "-o", executable, s_file, args.runtime],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"❌ Step 3 failed (clang): {test_name}")
        print(result.stderr)
        return False

    # Step 4: Run the executable and capture its output
    result = subprocess.run(
        [executable],
        input=test_input,
        encoding="ascii",
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"❌ Step 4 failed (executable): {test_name}")
        print(result.stderr)
        return False

    # Step 5: Compare the output with the expected output
    expected_output = test_output

    if result.stdout.strip() != expected_output.strip():
        print(f"❌ Test failed: {test_name}")
        print("Expected output:")
        print(expected_output)
        print("Actual output:")
        print(result.stdout)
        return False

    # If everything matches, the test passes
    if not quiet:
        print(f"✅ Test passed: {test_name}")
    return True


def main():
    source_test_dir = os.path.dirname(__file__)
    build_test_dir = os.path.join(args.build_dir, "tests")

    os.makedirs(build_test_dir, exist_ok=True)

    total_tests = 0
    passed_tests = 0

    # Iterate over all files in the test directory
    for root, _, files in os.walk(source_test_dir):
        for file in files:
            if file.endswith(".rkt"):
                # Get the full path to the .rkt file
                test_program = os.path.join(root, file)

                # Construct the paths to the .rkt.in and .rkt.out files
                test_input_file = os.path.join(root, file.replace(".rkt", ".rkt.in"))
                test_output_file = os.path.join(root, file.replace(".rkt", ".rkt.out"))
                err_file = os.path.join(root, file.replace(".rkt", ".rkt.err"))

                test_input = ""
                test_output = ""
                expect_compile_error = False
                # Check if the input and output files exist
                if os.path.exists(test_input_file):
                    test_input = open(test_input_file).read()
                if os.path.exists(test_output_file):
                    test_output = open(test_output_file).read()
                if os.path.exists(err_file):
                    expect_compile_error = True

                total_tests += 1
                # Run the test
                passed_tests += run_test(
                    test_program,
                    test_input,
                    test_output,
                    expect_compile_error,
                    build_test_dir,
                    quiet=True,
                )

    print(f"Passed {passed_tests}/{total_tests} tests.")


if __name__ == "__main__":
    main()
