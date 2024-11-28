#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

struct Node {
    char *value;
    struct Node *next;
};

typedef struct Node *List;

List list_new(void) {
    return NULL;
}

List list_add(List list, char *string) {
    struct Node *node = (struct Node*) malloc(sizeof(struct Node));
    node->value = (char*) malloc(strlen(string) + 1);
    strcpy(node->value, string);
    node->next = list;
    return node;
}

void list_print(List list) {
    struct Node *current = list;
    while (current != NULL) {
        printf("%s -> ", current->value);
        current = current->next;
    }
    printf("NULL\n");
}

void list_free(List list) {
    struct Node *current = list;
    while (current != NULL) {
        struct Node *next = current->next;
        free(current->value);
        free(current);
        current = next;
    }
}

int main(void) {
    List myList = list_new();
    myList = list_add(myList, "abc");
    myList = list_add(myList, "def");
    myList = list_add(myList, "ghi");

    list_print(myList);
    list_free(myList);

    return 0;
}



