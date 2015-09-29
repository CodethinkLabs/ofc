#include "../reformat.h"

void parse_reformat_print (FILE *file)
{
  char ch;
  unsigned col = 0, max_col = 132, indent = 0;
  char buffer[max_col];
  bool have_indent = false;

  while((ch = fgetc(file)) != EOF )
  {
    if ((col < max_col) && (ch != '\n'))
    {
      /* Column 0 means new line, a continuation line will
         start from column 7.*/
      if (col == 0)
      {
        indent = 0;
        have_indent = false;
      }

      if (!have_indent)
      {
        if ((col < 6) || (ch == ' '))
          indent++;
        else
          have_indent = true;
      }

      buffer[col++] = ch;
      continue;
    }

    if (ch == '\n')
    {
      buffer[col++] = ch;
      write(1, buffer, col);
      col = 0;
    }
    else if (col == max_col)
    {
      buffer[col++] = ch;
      unsigned cont_point = 0;
      unsigned cont_chars = 0;
      unsigned i;
      for (i = max_col; i > 0; i--)
      {
        if (isalnum(buffer[i]) > 0 || ((max_col - i) < 2))
        {
          cont_chars++;
        }
        else
        {
          cont_chars++;
          cont_point = i;
          break;
        }
      }

      char* end_of_line =
        (char*)malloc(sizeof(char) * cont_chars);

      strncpy(end_of_line, &buffer[cont_point], cont_chars);

      buffer[cont_point    ] = ' ';
      buffer[cont_point + 1] = '&';
      buffer[cont_point + 2] = '\n';

      write(1, buffer, cont_point + 3);

      for (i = 0; i < indent; i++)
        buffer[i] = ' ';

      col = indent;

      strncpy(&buffer[indent], end_of_line, cont_chars);

      col += cont_chars;
    }
  }
}
