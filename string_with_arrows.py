def string_with_arrows(text, start, end):
    result = ''

    # Get lines
    lines = text.split('\n')
    line = lines[start.line]
    
    # Calculate arrows
    arrow_line = ' ' * start.column + '^' * max(1, end.index - start.index)

    # Return the line with arrows under the error
    result += line + '\n' + arrow_line
    return result
