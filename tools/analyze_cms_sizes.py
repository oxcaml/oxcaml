#!/usr/bin/env python3
"""
Script to analyze and compare .cms file sizes between two builds.

Usage:
  Mode 1 (Aggregate): python3 analyze_cms_sizes.py aggregate <output_file> [search_dir]
  Mode 2 (Analyze):   python3 analyze_cms_sizes.py analyze <file1> <file2>

Modes:
  aggregate  Find all .cms files and save their sizes to output_file
  analyze    Compare two size files and show statistics
"""

import argparse
import os
import subprocess
import sys
import math

class BasicStats:
    def __init__(self, total_files, total_size, average_size, min_size, max_size):
        self.total_files = total_files
        self.total_size = total_size
        self.average_size = average_size
        self.min_size = min_size
        self.max_size = max_size

class FileChange:
    def __init__(self, path, new_size, old_size, absolute_change, percentage_change):
        self.path = path
        self.new_size = new_size
        self.old_size = old_size
        self.absolute_change = absolute_change
        self.percentage_change = percentage_change

def parse_size_file(filename):
    """Parse a file containing size and path pairs."""
    files = {}
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if line:
                parts = line.split(' ', 1)
                if len(parts) == 2:
                    size = int(parts[0])
                    path = parts[1]
                    files[path] = size
    return files

def calculate_basic_stats(files):
    """Calculate basic statistics for a set of files."""
    sizes = list(files.values())
    total_files = len(sizes)
    total_size = sum(sizes)
    average_size = total_size / total_files if total_files > 0 else 0
    
    return BasicStats(
        total_files=total_files,
        total_size=total_size,
        average_size=average_size,
        min_size=min(sizes) if sizes else 0,
        max_size=max(sizes) if sizes else 0
    )

def calculate_percentage_changes(new_files, old_files):
    """Calculate percentage changes for each file."""
    changes = []
    
    for path in new_files:
        if path in old_files:
            new_size = new_files[path]
            old_size = old_files[path]
            
            if old_size > 0:
                percentage_change = ((new_size - old_size) / old_size) * 100
                changes.append(FileChange(
                    path=path,
                    new_size=new_size,
                    old_size=old_size,
                    absolute_change=new_size - old_size,
                    percentage_change=percentage_change
                ))
    
    return changes

def find_outliers(changes, top_n=10):
    """Find the top N files with largest percentage increases and decreases."""
    increases = sorted(changes, key=lambda x: x.percentage_change, reverse=True)[:top_n]
    decreases = sorted(changes, key=lambda x: x.percentage_change)[:top_n]
    
    return increases, decreases

def calculate_std_dev(values, mean):
    """Calculate standard deviation."""
    if len(values) <= 1:
        return 0.0
    variance = sum((x - mean) ** 2 for x in values) / (len(values) - 1)
    return math.sqrt(variance)

def create_histogram(percentage_changes, bins=10, title="Size Change Distribution", show_stats=True, show_detailed_for_first_bucket=False):
    """Create a text-based histogram of percentage changes."""
    if not percentage_changes:
        return
    
    min_change = min(percentage_changes)
    max_change = max(percentage_changes)
    
    # Create bins
    bin_width = (max_change - min_change) / bins
    bin_counts = [0] * bins
    bin_ranges = []
    
    for i in range(bins):
        bin_start = min_change + i * bin_width
        bin_end = min_change + (i + 1) * bin_width
        bin_ranges.append((bin_start, bin_end))
    
    # Count occurrences in each bin
    for change in percentage_changes:
        if change >= max_change:  # Handle edge case for max value
            bin_index = bins - 1
        else:
            bin_index = int((change - min_change) / bin_width)
        bin_counts[bin_index] += 1
    
    # Find max count for scaling
    max_count = max(bin_counts) if bin_counts else 0
    if max_count == 0:
        return
    
    # Create histogram
    if "Detailed" in title:
        print(f"\n{title} ({len(percentage_changes)} files):")
    else:
        print(f"\n{title}:")
    print(f"Range: {min_change:.1f}% to {max_change:.1f}%")
    
    # Scale to 40 characters max width
    scale = 40 / max_count
    
    for i, (count, (bin_start, bin_end)) in enumerate(zip(bin_counts, bin_ranges)):
        bar_length = int(count * scale)
        bar = '█' * bar_length
        percentage = (count / len(percentage_changes)) * 100
        print(f"{bin_start:6.1f}% - {bin_end:6.1f}%: {count:3d} ({percentage:4.1f}%) {bar}")
    
    # Statistical summary
    if show_stats:
        mean_change = sum(percentage_changes) / len(percentage_changes)
        sorted_changes = sorted(percentage_changes)
        median_change = sorted_changes[len(sorted_changes) // 2]
        print(f"\nMedian: {median_change:.2f}%, Std dev: {calculate_std_dev(percentage_changes, mean_change):.1f}%")
    
    # Create detailed histogram for first bucket
    if show_detailed_for_first_bucket:
        first_bucket_max = min_change + bin_width
        first_bucket_changes = [c for c in percentage_changes if c <= first_bucket_max]
        if len(first_bucket_changes) > 10:  # Only if there are enough files to make it worthwhile
            create_histogram(first_bucket_changes, bins=10, title="Detailed view of main group", 
                           show_stats=False, show_detailed_for_first_bucket=False)

def aggregate_cms_files(output_file, search_dir="."):
    """Find all .cms files and save their sizes to output_file."""
    print(f"Searching for .cms files in {os.path.abspath(search_dir)}...")
    
    try:
        # Use find command to locate all .cms files and get their sizes
        cmd = ['find', search_dir, '-name', '*.cms', '-type', 'f', '-exec', 'ls', '-la', '{}', ';']
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, 
                                universal_newlines=True, check=True)
        
        files_info = []
        for line in result.stdout.strip().split('\n'):
            if line:
                parts = line.split()
                if len(parts) >= 9:
                    size = parts[4]
                    path = ' '.join(parts[8:])  # Handle paths with spaces
                    files_info.append((int(size), path))
        
        # Sort by path for consistent output
        files_info.sort(key=lambda x: x[1])
        
        # Write to output file
        with open(output_file, 'w') as f:
            for size, path in files_info:
                f.write(f"{size} {path}\n")
        
        print(f"Found {len(files_info)} .cms files")
        print(f"Saved size information to {output_file}")
        
        # Show some basic stats
        if files_info:
            total_size = sum(size for size, _ in files_info)
            avg_size = total_size / len(files_info)
            print(f"Total size: {total_size:,} bytes")
            print(f"Average size: {avg_size:.1f} bytes")
        
    except subprocess.CalledProcessError as e:
        print(f"Error running find command: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

def analyze_size_files(file1, file2):
    """Analyze and compare two size files."""
    print("CMS File Size Analysis")
    print("=" * 50)
    
    try:
        new_files = parse_size_file(file1)
        old_files = parse_size_file(file2)
    except FileNotFoundError as e:
        print(f"Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing files: {e}")
        sys.exit(1)
    
    new_stats = calculate_basic_stats(new_files)
    old_stats = calculate_basic_stats(old_files)
    changes = calculate_percentage_changes(new_files, old_files)
    
    if not changes:
        print("No matching files found!")
        return
    
    # Summary statistics
    avg_percentage_change = sum(c.percentage_change for c in changes) / len(changes)
    total_size_increase = new_stats.total_size - old_stats.total_size
    total_percentage_increase = (total_size_increase / old_stats.total_size) * 100 if old_stats.total_size > 0 else 0
    
    print(f"Files analyzed: {len(changes)}")
    print(f"Total size change: {total_size_increase:+,} bytes ({total_percentage_increase:+.2f}%)")
    print(f"Average per-file change: {avg_percentage_change:+.2f}%")
    
    # Generate histogram
    percentage_changes = [c.percentage_change for c in changes]
    create_histogram(percentage_changes, show_detailed_for_first_bucket=True)
    
    # Top outliers
    increases, decreases = find_outliers(changes, 5)
    
    print(f"\nTop size increases:")
    for i, change in enumerate(increases, 1):
        filename = change.path.split('/')[-1]
        print(f"{i}. {filename}: +{change.percentage_change:.1f}% ({change.old_size:,} → {change.new_size:,} bytes)")
    
    if any(c.percentage_change < 0 for c in decreases):
        print(f"\nTop size decreases:")
        for i, change in enumerate([c for c in decreases if c.percentage_change < 0][:5], 1):
            filename = change.path.split('/')[-1]
            print(f"{i}. {filename}: {change.percentage_change:.1f}% ({change.old_size:,} → {change.new_size:,} bytes)")

def main():
    parser = argparse.ArgumentParser(
        description='Analyze and compare .cms file sizes',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Aggregate .cms files from current directory
  python3 analyze_cms_sizes.py aggregate sizes.txt
  
  # Aggregate from specific directory
  python3 analyze_cms_sizes.py aggregate sizes.txt /path/to/build
  
  # Compare two size files
  python3 analyze_cms_sizes.py analyze sizes-new.txt sizes-old.txt
        """
    )
    
    subparsers = parser.add_subparsers(dest='mode', help='Operation mode')
    
    # Aggregate mode
    agg_parser = subparsers.add_parser('aggregate', help='Find .cms files and save sizes')
    agg_parser.add_argument('output_file', help='Output file to save size information')
    agg_parser.add_argument('search_dir', nargs='?', default='.', 
                           help='Directory to search for .cms files (default: current directory)')
    
    # Analyze mode
    analyze_parser = subparsers.add_parser('analyze', help='Analyze two size files')
    analyze_parser.add_argument('file1', help='First size file (newer/modified version)')
    analyze_parser.add_argument('file2', help='Second size file (older/baseline version)')
    
    args = parser.parse_args()
    
    if args.mode == 'aggregate':
        aggregate_cms_files(args.output_file, args.search_dir)
    elif args.mode == 'analyze':
        analyze_size_files(args.file1, args.file2)
    else:
        parser.print_help()
        sys.exit(1)

if __name__ == '__main__':
    main()