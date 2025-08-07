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
    
    return {
        'total_files': total_files,
        'total_size': total_size,
        'average_size': average_size,
        'min_size': min(sizes) if sizes else 0,
        'max_size': max(sizes) if sizes else 0
    }

def calculate_percentage_changes(new_files, old_files):
    """Calculate percentage changes for each file."""
    changes = []
    
    for path in new_files:
        if path in old_files:
            new_size = new_files[path]
            old_size = old_files[path]
            
            if old_size > 0:
                percentage_change = ((new_size - old_size) / old_size) * 100
                changes.append({
                    'path': path,
                    'new_size': new_size,
                    'old_size': old_size,
                    'absolute_change': new_size - old_size,
                    'percentage_change': percentage_change
                })
    
    return changes

def find_outliers(changes, top_n=10):
    """Find the top N files with largest percentage increases and decreases."""
    increases = sorted(changes, key=lambda x: x['percentage_change'], reverse=True)[:top_n]
    decreases = sorted(changes, key=lambda x: x['percentage_change'])[:top_n]
    
    return increases, decreases

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
    
    # Load data
    print("Loading data...")
    try:
        new_files = parse_size_file(file1)
        old_files = parse_size_file(file2)
    except FileNotFoundError as e:
        print(f"Error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error parsing files: {e}")
        sys.exit(1)
    
    # Basic statistics
    print("\nBasic Statistics:")
    print("-" * 20)
    
    new_stats = calculate_basic_stats(new_files)
    old_stats = calculate_basic_stats(old_files)
    
    print(f"File 1 ({file1}) files: {new_stats['total_files']}")
    print(f"File 2 ({file2}) files: {old_stats['total_files']}")
    print(f"File 1 average size: {new_stats['average_size']:.1f} bytes")
    print(f"File 2 average size: {old_stats['average_size']:.1f} bytes")
    print(f"File 1 total size: {new_stats['total_size']:,} bytes")
    print(f"File 2 total size: {old_stats['total_size']:,} bytes")
    
    # Calculate percentage changes
    changes = calculate_percentage_changes(new_files, old_files)
    
    if not changes:
        print("No matching files found!")
        return
    
    # Average percentage change and total increase
    avg_percentage_change = sum(c['percentage_change'] for c in changes) / len(changes)
    total_size_increase = new_stats['total_size'] - old_stats['total_size']
    total_percentage_increase = (total_size_increase / old_stats['total_size']) * 100 if old_stats['total_size'] > 0 else 0
    
    print(f"\nSummary:")
    print("-" * 20)
    print(f"Average percentage change per file: {avg_percentage_change:.5f}%")
    if total_size_increase >= 0:
        print(f"Total size increase: {total_size_increase:,} bytes")
        print(f"Total percentage increase: {total_percentage_increase:.5f}%")
    else:
        print(f"Total size decrease: {abs(total_size_increase):,} bytes")
        print(f"Total percentage decrease: {abs(total_percentage_increase):.5f}%")
    
    # Find outliers
    increases, decreases = find_outliers(changes)
    
    print(f"\nTop {len(increases)} files with largest percentage increases:")
    print("-" * 60)
    for i, change in enumerate(increases, 1):
        filename = change['path'].split('/')[-1]
        print(f"{i:2d}. {filename}: +{change['percentage_change']:.3f}% "
              f"({change['old_size']:,} → {change['new_size']:,} bytes)")
    
    print(f"\nTop {len(decreases)} files with largest percentage decreases:")
    print("-" * 60)
    for i, change in enumerate(decreases, 1):
        if change['percentage_change'] < 0:
            filename = change['path'].split('/')[-1]
            print(f"{i:2d}. {filename}: {change['percentage_change']:.3f}% "
                  f"({change['old_size']:,} → {change['new_size']:,} bytes)")
    
    # Additional verification info
    print(f"\nVerification Information:")
    print("-" * 30)
    print(f"Files analyzed: {len(changes)}")
    print(f"Files only in new: {len(new_files) - len(changes)}")
    print(f"Files only in old: {len(old_files) - len(changes)}")
    
    # Largest absolute changes
    abs_increases = sorted(changes, key=lambda x: x['absolute_change'], reverse=True)[:5]
    abs_decreases = sorted(changes, key=lambda x: x['absolute_change'])[:5]
    
    print(f"\nLargest absolute size increases:")
    for change in abs_increases:
        if change['absolute_change'] > 0:
            filename = change['path'].split('/')[-1]
            print(f"  {filename}: +{change['absolute_change']:,} bytes")
    
    print(f"\nLargest absolute size decreases:")
    for change in abs_decreases:
        if change['absolute_change'] < 0:
            filename = change['path'].split('/')[-1]
            print(f"  {filename}: {change['absolute_change']:,} bytes")

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