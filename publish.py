import os
import zipfile
import re
    
pattern = re.compile("(?:(v) : )?(.*) : (-?[0-9]+\.?[0-9]*|\.[0-9]+)") 

def check_file_names(files):
    for file in files:
        text_file = open(os.path.join("test", file), 'r')
        text = text_file.read()
        text_file.close()

        test_cases = re.findall(r'(SC.testProperty|QC.testProperty|testCase) "(.*)"', text)

        for case in test_cases:
            print("Found:", case[0], "with description", case[1], end=' ')

            result = pattern.match(case[1])
            if result:
                print("\033[92mValid test\033[0m")
            else:
                print("\033[91mTest has an error with its description, please revise\033[0m")


zf = zipfile.ZipFile("autograder.zip", "w")
for dirname, _, files in os.walk("."):
    if '.stack-work' in dirname or '.git' in dirname:
        continue
    elif 'test' in dirname:
        check_file_names(files)

    zf.write(dirname)
    for filename in files:
        if '.zip' in filename or '.gitignore' in filename:
            continue
        zf.write(os.path.join(dirname, filename))

print("Successfully zipped autograder")

zf.close()