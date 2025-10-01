#!/usr/bin/env python3
"""
Filter the 2dx_master.cfg file to keep only parameters for 2D Electron Crystallography (mode 1)
"""

import re

def filter_config_file(input_file, output_file):
    with open(input_file, 'r') as f:
        lines = f.readlines()
    
    filtered_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        
        # Look for parameter blocks that start with # LABEL:
        if line.startswith('# LABEL:'):
            # Collect the entire parameter block
            block_lines = []
            j = i
            
            # Read until we find a 'set' line or another block starts
            while j < len(lines):
                block_lines.append(lines[j])
                
                # Check if this line contains MODE specification
                if lines[j].startswith('# MODE:'):
                    mode_line = lines[j].strip()
                    mode_value = mode_line.replace('# MODE:', '').strip()
                    
                    # If mode contains '1' (2D Electron Crystallography), keep the block
                    # If mode is "ALL" or doesn't restrict modes, keep it
                    if '1' in mode_value or mode_value == 'ALL':
                        keep_block = True
                    else:
                        keep_block = False
                    break
                    
                # If we reach a 'set' line without finding MODE, keep the block (no mode restriction)
                if lines[j].startswith('set '):
                    keep_block = True
                    break
                    
                j += 1
                
                # If we reach the end or another parameter block, stop
                if j < len(lines) and (lines[j].startswith('# LABEL:') or lines[j].startswith('#=')):
                    break
            
            # Continue reading until we find the 'set' line for this parameter
            while j < len(lines) and not lines[j].startswith('set '):
                block_lines.append(lines[j])
                j += 1
            
            # Add the 'set' line if we found it
            if j < len(lines) and lines[j].startswith('set '):
                block_lines.append(lines[j])
                j += 1
            
            # Add the block if we should keep it
            if 'keep_block' in locals() and keep_block:
                filtered_lines.extend(block_lines)
            
            i = j
        else:
            # For non-parameter lines (comments, headers, etc.), keep them
            if not line.startswith('set ') or not any(filtered_lines[k].startswith('# LABEL:') for k in range(max(0, len(filtered_lines)-10), len(filtered_lines))):
                filtered_lines.append(line)
            i += 1
    
    # Write the filtered content
    with open(output_file, 'w') as f:
        f.writelines(filtered_lines)

if __name__ == "__main__":
    input_file = "/home/weindel/Projects/focus/apps/resources/config/2dx_master.cfg"
    output_file = "/home/weindel/Projects/focus/apps/resources/config/2dx_master.cfg.filtered"
    
    filter_config_file(input_file, output_file)
    print(f"Filtered configuration saved to {output_file}")