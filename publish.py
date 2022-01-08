import os
import zipfile
import re
    
pattern = re.compile("(?:(v) : )?(.*) : (-?[0-9]+\.?[0-9]*|\.[0-9]+)") 

def check_tests(file):
    """Check that Spec.hs will work on gradescope."""
    test_path = os.path.join("test", file)
    text_file = open(test_path, 'r')

    text = ""
    for line in text_file:
        # This ensures tests will be pushed to xml on gradescope
        if "main = defaultMain tests\n" == line:
            line = "main = defaultMainWithIngredients [antXMLRunner] tests -- main = defaultMain tests\n"
        text += line

    text_file.close()

    if file == "Spec.hs":
        os.rename(test_path, os.path.join("test", "temp.hs"))

        writing_file = open(test_path, 'w')
        writing_file.write(text)
        writing_file.close()

    test_cases = re.findall(r'(SC.testProperty|QC.testProperty|testCase) "(.*)"', text)

    for case in test_cases:
        print("Found:", case[0], "with description", case[1], end=' ')

        result = pattern.match(case[1])
        if result:
            print("\033[92mValid test\033[0m") # Green
        else:
            print("\033[91mTest has an error with its description, please revise\033[0m") # Red


def write_directory(zf, dirname, files):
    zf.write(dirname)
    for filename in files:
        if '.zip' in filename or '.gitignore' in filename:
            continue
        zf.write(os.path.join(dirname, filename))

def create_zip():
    zf = zipfile.ZipFile("autograder.zip", "w")
    for dirname, _, files in os.walk("."):
        if '.stack-work' in dirname or '.git' in dirname:
            continue
        elif 'test' in dirname:
            if "Spec.hs" not in files:
                raise ValueError('Test folder is missing Spec.hs')
            for file in files:
                check_tests(file)
            write_directory(zf, dirname, files)
            os.remove(os.path.join("test", "Spec.hs"))
            os.rename(os.path.join("test", "temp.hs"), os.path.join("test", "Spec.hs"))
        else:
            write_directory(zf, dirname, files)

    print("Successfully zipped autograder")

    zf.close()

if __name__ == "__main__":
    create_zip()