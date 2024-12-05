import os
from argparse import ArgumentParser
import requests

def create_placeholder(day, path):
    with open(path, "w", encoding="utf-8") as fp:
        fp.write(f"module Day{day}\n\n")
        fp.write("let part1 (lines: string list) =\n")
        fp.write("    0\n\n")
        fp.write("let part2 (lines: string list) =\n")
        fp.write("    0\n")

def add_to_fsproj(day, code_path, proj_path):
    new_lines = []
    prev_day = day - 1
    with open(proj_path, "r", encoding="utf-8") as fp:
        for line in fp:
            new_lines.append(line)

            if rf'Include="Day{prev_day}\Day{prev_day}.fs"' in line:
                new_lines.append(f'    <Compile Include="{code_path}" />\n')

    with open(proj_path, "w", encoding="utf-8") as fp:
        for line in new_lines:
            fp.write(line)

def add_to_main(day, path):
    new_lines = []
    prev_day = day - 1
    with open(path, "r", encoding="utf-8") as fp:
        for line in fp:
            found_line = f"[Day{prev_day}.part1; Day{prev_day}.part2]" in line

            if found_line:
                line = "    " + line.strip() + ";\n"

            new_lines.append(line)

            if found_line:
                new_lines.append(f"    [Day{day}.part1; Day{day}.part2]\n")

    with open(path, "w", encoding="utf-8") as fp:
        for line in new_lines:
            fp.write(line)

def download_input(day, path):
    print("Downloading input text from adventofcode.com...")

    # Download input from adventofcode
    url = f"https://adventofcode.com/2024/day/{day}/input"

    # Get session ID and attach it as a cookie to the request
    with open("secret.txt", "r", encoding="utf-8") as fp:
        session_id = fp.readline().strip()

    input_text = requests.get(url, cookies={"session": session_id}).text

    # Write input text to file
    with open(path, "w", encoding="utf-8") as fp:
        fp.write(input_text)

parser = ArgumentParser()

parser.add_argument("day", type=int)

args = parser.parse_args()

folder = f"Day{args.day}"
if os.path.exists(folder):
    print(f"Folder already exists for day {args.day}. Skipping...")
else:
    os.mkdir(folder)

code_file = rf"{folder}\{folder}.fs"
if os.path.exists(code_file):
    print(f"Code file already exists for day {args.day}. Skipping...")
else:
    print(f"Creating placeholder F# file at '{code_file}'")
    create_placeholder(args.day, code_file)
    print(f"Adding file to fsproj file at 'AOC2024.fsproj'")
    add_to_fsproj(args.day, code_file, "AOC2024.fsproj")
    print(f"Adding reference to file in 'Main.fs'")
    add_to_main(args.day, "Main.fs")

input_path = f"{folder}/input.txt"
if os.path.exists(input_path):
    print(f"Input file already exists for day {args.day}. Exiting...")
    exit(0)
else:
    download_input(args.day, input_path)
