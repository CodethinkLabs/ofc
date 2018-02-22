#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2018 Codethink Ltd.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import argparse
import os
import re
import string
import ntpath

rep_page_index = 0

def is_valid_file(file_path):
    """
    'Type' for argparse - checks that file exists but does not open.
    """
    if not os.path.exists(file_path):
        raise argparse.ArgumentError(
            "{0} does not exist".format(file_path))
    return file_path

def is_valid_directory(dir_path):
    if not os.path.isdir(dir_path):
        try:
            os.makedirs(dir_path)
        except OSError as exception:
            if exception.errno != errno.EEXIST:
                raise
            else:
                dir_path = dir_path  + datetime.datetime.now().isoformat()
                os.makedirs(dir_path)
    return dir_path

def add_to_stderr_dict(stderr_dict, text):
    identifier = ""
    for line in text:
        # The lines which end in ":" are paths to the files where the
        # error or warning is. The following line to the last line which
        # contains ":", is the description of the error/warning and
        # it will be used as identifier for this error/warning
        # (dictionary key).
        if line.endswith(":"):
            continue

        # OFC output enclose some words in sigle quotes which are variable
        # names or types which makes the same error/warning being different,
        # removing this words creates the real identifier.
        identifier = re.sub('\'(\S+?)\'', "", line.strip())
        # TODO: remove ^ from the identifier, if this happens probably the
        #       text is just the character "^" with some spaces, and probably
        #       it has happening because there were 2 ^ in consecutive lines?
        #       Removing this identifier will avoid to create an entrance in
        #       the dictionary for this and therefore stops creating a file
        #       with no information.
        identifier = re.sub('\^', "", identifier)
        identifier = re.sub('[^0-9a-zA-Z]+', "_", identifier)
        # Merge warning/errors which only differs because digits
        identifier = re.sub('\d', "", identifier)
        break

    if identifier.strip() != "":
        if identifier not in stderr_dict.keys():
            stderr_dict[str(identifier)] = []

        stderr_dict[str(identifier)].append(text)

def find_filename(path):
    head, tail = ntpath.split(path)
    return tail or ntpath.basename(head)

def write_message_in_separate_worksheet(workbook, message, ocurrences):
    bold = workbook.add_format({'bold': True, 'font_name': 'Courier'})
    courier_font = workbook.add_format({'font_name': 'Courier'})
    stripped_message = message[0:26]
    if not stripped_message:
        return
    try:
        # Hack because sheets can not have more than 30 characters
        worksheet = workbook.add_worksheet(stripped_message + "...")
    except:
        # Hack because there are similar messages if we use less than 30 characters.
        global rep_page_index
        rep_page_index += 1
        worksheet = workbook.add_worksheet(stripped_message + "_" + str(rep_page_index) + "...")

    row_number = 1
    worksheet.write_row('A' + str(row_number), [ message + " ocurrences" ], bold)

    for text in ocurrences:
        for line in text:
            row_number +=1
            row = [ str(line) ]
            worksheet.write_row('A' + str(row_number), row, courier_font)

def write_to_xlsx(stderr_dict, output_path, input_file_path):
    import xlsxwriter
    row_number = 1
    headings = [
        'Error/Warning message',
        'Number of Occurrences',
    ]

    # Open workbook and worksheet
    output_filename = find_filename(input_file_path) + "_classified.xlsx"
    output_workbook = os.path.join(output_dir, output_filename)
    workbook = xlsxwriter.Workbook(output_workbook)
    bold = workbook.add_format({'bold': True, 'font_name': 'Courier'})
    courier_font = workbook.add_format({'font_name': 'Courier'})

    # Add summary worksheet
    worksheet = workbook.add_worksheet('Summary')
    worksheet.write_row('A' + str(row_number), ['OFC stderr Analysis'], bold)
    row_number += 2

    # Fill up the summary table
    worksheet.write_row('A' + str(row_number), headings, bold)
    for message, ocurrences in stderr_dict.iteritems():
        row_number += 1
        row = [ message, len(ocurrences) ]
        worksheet.write_row('A' + str(row_number), row, courier_font)
        write_message_in_separate_worksheet(workbook, message, ocurrences)

def write_to_plain_text(stderr_dict, output_dir, input_file_path):
    output_filename = "SUMMARY_" + find_filename(input_file_path) + ".txt"
    output_filepath = os.path.join(output_dir, output_filename)
    # Write the summary in cvs ";" separated format
    with open(output_filepath, 'w') as f:
        heading = "Error/Warning Message;Number of Occurrences\n"
        f.write(heading)
        f.write("=" * len(heading))
        f.write("\n")
        for message, ocurrences in stderr_dict.iteritems():
            f.write("%s;%s\n" % (message, str(len(ocurrences))))
            filename = str(message) + ".txt"
            filepath = os.path.join(output_dir, filename.replace(" ", "_"))
            with open(filepath, 'w') as sf:
                for text in ocurrences:
                    for line in text:
                        sf.write(line + "\n")

parser = argparse.ArgumentParser(
                description='Classify and count warnings and errors in OFC stderr files')
parser.add_argument('--excel', action='store_true',
    help="""The OFC stderr analysis output will be an excel file, if not it
            will be multiple files in plain text""")
parser.add_argument('-f', '--filename',
    dest="filename", required=True, type=is_valid_file,
    help="""OFC stderr log file, produced when running OFC on a file or
            set of them""",
    metavar="FILE")
parser.add_argument('-o','--output_dir',
    dest="output_dir", required=False, type=is_valid_directory,
    help="Output directory where to find result files",
    metavar="DIR")
args = parser.parse_args()

if not args.output_dir:
    output_dir = "output"
    is_valid_directory(output_dir)
else:
    output_dir = args.output_dir

with open(args.filename, 'r') as f:
    content = f.read().splitlines()

set_lines = []
count = 0
stderr_dict = {}
for line in content:
    # Filter out non valid lines
    if not line:
        continue

    # Remove any character non printable
    line = line.decode("ascii", 'ignore')
    line = filter(string.printable.__contains__, line)

    # Add lines into a list which can be parsed later
    set_lines.append(line)

    # OFC stderr files (warnings and errors) use the "^" character to
    # point the position in the line where the warning/error happens.
    # This character appears only in the latest line of the error before
    # a blank line so can be used as token to define the end of the error/warning
    # message.
    if "^" not in line:
        continue
    else:
        add_to_stderr_dict(stderr_dict, set_lines)
        # Prepare for the next text
        set_lines = []

# Write the results to files
if args.excel == True:
    write_to_xlsx(stderr_dict, output_dir, args.filename)
else:
    write_to_plain_text(stderr_dict, output_dir, args.filename)
