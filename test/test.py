import os
import subprocess
import glob

failed = "NOT CORRECT"
passed = "CORRECT"

def main():
    os.chdir("../")

    # Get a list of all files in the current directory
    files = sorted(glob.glob('./examples/**/*.vpr'))

    results = {}

    for file in files:
        cmd = ['cargo', 'run', file]
        # print(f'Executing: {" ".join(cmd)}')
        
        try:
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT, text=True)
            expected = file[file.rfind('-')+1:-4]
            if 'Verified' in output and expected == 'true':
                results[file] = passed
            elif 'Unverified' in output and expected == 'false':
                results[file] = passed
            else:
                results[file] = failed
        except subprocess.CalledProcessError:
            print(f"Error executing command for file {file}")

    if failed in results.values():
        print("These tests failed: ")
        for key in results.keys():
            if results[key] == failed:
                print("    "+key)
    else:
        print("All tests passed")

if __name__ == "__main__":
    main()