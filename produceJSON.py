import xml.etree.ElementTree as ET
import re
import json
import os

# match any word with a " : " between any numbers. Option to start with "v : "
pattern = re.compile("(?:(v) : )?(.*) : (-?[0-9]+\.?[0-9]*|\.[0-9]+)") 

def extract_test_info(test_name):
    result = pattern.match(test_name)
    if result:
        return (result.group(1) is not None), result.group(2), float(result.group(3))

    return True, "issue parsing test", 0.0

def xml_to_json(file_name):
    tree = ET.parse(file_name)
    root = tree.getroot()

    conversion = {"execution_time": float(root.attrib['time']), 
        "visibility": "after_published",
        "stdout_visibility": "after_published"}

    tests = []
    
    for test_suite in root.iter('testsuite'):
        suite_name = test_suite.attrib['name'] + ": "
        test_cases = test_suite.findall('testcase')

        for case in test_cases:
            visibility, name, marks = extract_test_info(case.attrib['name'])
            
            test_info = {"name": suite_name + name, 
                "visibility": "visible" if visibility else "after_published",
                "output": "Success!", "score": marks, "max_score": marks}

            fail_status = case.findall('failure')
            if len(fail_status) > 0:
                test_info['score'] = 0.0
                test_info['output'] = fail_status[0].text

            tests.append(test_info)
    
    conversion["tests"] = tests

    with open('/autograder/results/results.json', 'w') as f:
        json.dump(conversion, f)

def check_test_run(file_name):
    if os.path.isfile(file_name):
        return True
    
    conversion = {"score": 0.0,
        "visibility": "after_published",
        "stdout_visibility": "visible"}
        
    with open('/autograder/results/results.json', 'w') as f:
        json.dump(conversion, f)

    return False

if __name__ == "__main__":
    file_name = "test.xml"

    status = check_test_run(file_name)
    if status:
        xml_to_json(file_name)
    
