#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

#include "cmark.h"

/*
 * Simple wrapper around cmark. Tried using cmark itself but it hung --
 * I think it wasn't sure whether or not to begin parsing or wait for
 * more input.
 *
 * This also has the advantage of not having to open new OS processes
 * for every call.
 */
int
main() {

    int stdin_fd = fileno(stdin);   // Other than 0 I eat my hat.
    int stdout_fd = fileno(stdin);  // Other than 1 I eat my hat.
    int stderr_fd = fileno(stderr); // Other than 2 I eat my hat.

    unsigned num_bytes = 4;
    uint8_t buffer[4];
    memset(buffer, '0', num_bytes);
    while (read(stdin_fd, buffer, num_bytes) > 0) {
        uint32_t input_length = 0;
        memcpy(&input_length, buffer, num_bytes);

        char* input_buffer = (char*) malloc(input_length * sizeof(char));
        char* result;
        if (read(stdin_fd, input_buffer, input_length) > 0) {
            result = cmark_markdown_to_html(input_buffer, input_length, CMARK_OPT_DEFAULT);
        } else {
            fprintf(stderr, "Failure to parse markdown");
        }
        fprintf(stdout, "%s", result);
        fflush(stdout);
        free(input_buffer);
    }
}
