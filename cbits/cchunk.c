#include <stdlib.h>
#include <time.h>
#include <stdio.h>

enum {
    EAST,
    WEST,
    TOP,
    BOTTOM,
    NORTH,
    SOUTH
};

void appendFace(const int n, const char face,char* surface,const char x, const char y, const char z, const char w) {
    switch(face) {
//     {1,1,0,0
//     ,1,1,1,0
//     ,1,0,1,0
//     ,1,0,1,0
//     ,1,0,0,0
//     ,1,1,0,0
//     };
        case (EAST):
            surface[n*24+ 0] = x + 1; surface[n*24+ 1] = y + 1; surface[n*24+ 2] = z + 0; surface[n*24+ 3] = w + 0;
            surface[n*24+ 4] = x + 1; surface[n*24+ 5] = y + 1; surface[n*24+ 6] = z + 1; surface[n*24+ 7] = w + 0;
            surface[n*24+ 8] = x + 1; surface[n*24+ 9] = y + 0; surface[n*24+10] = z + 1; surface[n*24+11] = w + 0;
            surface[n*24+12] = x + 1; surface[n*24+13] = y + 0; surface[n*24+14] = z + 1; surface[n*24+15] = w + 0;
            surface[n*24+16] = x + 1; surface[n*24+17] = y + 0; surface[n*24+18] = z + 0; surface[n*24+19] = w + 0;
            surface[n*24+20] = x + 1; surface[n*24+21] = y + 1; surface[n*24+22] = z + 0; surface[n*24+23] = w + 0;
            break;
                
//     {0,0,1,0
//     ,0,1,1,0
//     ,0,1,0,0
//     ,0,1,0,0
//     ,0,0,0,0
//     ,0,0,1,0
//     };
        case (WEST):
            surface[n*24+ 0] = x + 0; surface[n*24+ 1] = y + 0; surface[n*24+ 2] = z + 1; surface[n*24+ 3] = w + 0;
            surface[n*24+ 4] = x + 0; surface[n*24+ 5] = y + 1; surface[n*24+ 6] = z + 1; surface[n*24+ 7] = w + 0;
            surface[n*24+ 8] = x + 0; surface[n*24+ 9] = y + 1; surface[n*24+10] = z + 0; surface[n*24+11] = w + 0;
            surface[n*24+12] = x + 0; surface[n*24+13] = y + 1; surface[n*24+14] = z + 0; surface[n*24+15] = w + 0;
            surface[n*24+16] = x + 0; surface[n*24+17] = y + 0; surface[n*24+18] = z + 0; surface[n*24+19] = w + 0;
            surface[n*24+20] = x + 0; surface[n*24+21] = y + 0; surface[n*24+22] = z + 1; surface[n*24+23] = w + 0;
            break;

//     {0,1,0,-16
//     ,0,1,1,-16
//     ,1,1,1,-16
//     ,1,1,1,-16
//     ,1,1,0,-16
//     ,0,1,0,-16
//     };
        case (TOP):
            surface[n*24+ 0] = x + 0; surface[n*24+ 1] = y + 1; surface[n*24+ 2] = z + 0; surface[n*24+ 3] = w - 16;
            surface[n*24+ 4] = x + 0; surface[n*24+ 5] = y + 1; surface[n*24+ 6] = z + 1; surface[n*24+ 7] = w - 16;
            surface[n*24+ 8] = x + 1; surface[n*24+ 9] = y + 1; surface[n*24+10] = z + 1; surface[n*24+11] = w - 16;
            surface[n*24+12] = x + 1; surface[n*24+13] = y + 1; surface[n*24+14] = z + 1; surface[n*24+15] = w - 16;
            surface[n*24+16] = x + 1; surface[n*24+17] = y + 1; surface[n*24+18] = z + 0; surface[n*24+19] = w - 16;
            surface[n*24+20] = x + 0; surface[n*24+21] = y + 1; surface[n*24+22] = z + 0; surface[n*24+23] = w - 16;
            break;

//    {1,0,1,-16
//    ,0,0,1,-16
//    ,0,0,0,-16
//    ,0,0,0,-16
//    ,1,0,0,-16
//    ,1,0,1,-16
//    };
        case(BOTTOM):
            surface[n*24+ 0] = x + 1; surface[n*24+ 1] = y + 0; surface[n*24+ 2] = z + 1; surface[n*24+ 3] = w - 16;
            surface[n*24+ 4] = x + 0; surface[n*24+ 5] = y + 0; surface[n*24+ 6] = z + 1; surface[n*24+ 7] = w - 16;
            surface[n*24+ 8] = x + 0; surface[n*24+ 9] = y + 0; surface[n*24+10] = z + 0; surface[n*24+11] = w - 16;
            surface[n*24+12] = x + 0; surface[n*24+13] = y + 0; surface[n*24+14] = z + 0; surface[n*24+15] = w - 16;
            surface[n*24+16] = x + 1; surface[n*24+17] = y + 0; surface[n*24+18] = z + 0; surface[n*24+19] = w - 16;
            surface[n*24+20] = x + 1; surface[n*24+21] = y + 0; surface[n*24+22] = z + 1; surface[n*24+23] = w - 16;
            break;

//     {1,1,1,0
//     ,0,1,1,0
//     ,0,0,1,0
//     ,0,0,1,0
//     ,1,0,1,0
//     ,1,1,1,0
//     };
        case(NORTH):
            surface[n*24+ 0] = x + 1; surface[n*24+ 1] = y + 1; surface[n*24+ 2] = z + 1; surface[n*24+ 3] = w + 0;
            surface[n*24+ 4] = x + 0; surface[n*24+ 5] = y + 1; surface[n*24+ 6] = z + 1; surface[n*24+ 7] = w + 0;
            surface[n*24+ 8] = x + 0; surface[n*24+ 9] = y + 0; surface[n*24+10] = z + 1; surface[n*24+11] = w + 0;
            surface[n*24+12] = x + 0; surface[n*24+13] = y + 0; surface[n*24+14] = z + 1; surface[n*24+15] = w + 0;
            surface[n*24+16] = x + 1; surface[n*24+17] = y + 0; surface[n*24+18] = z + 1; surface[n*24+19] = w + 0;
            surface[n*24+20] = x + 1; surface[n*24+21] = y + 1; surface[n*24+22] = z + 1; surface[n*24+23] = w + 0;
            break;

//     {0,0,0,0
//     ,0,1,0,0
//     ,1,1,0,0
//     ,1,1,0,0
//     ,1,0,0,0
//     ,0,0,0,0
//     };
        case(SOUTH):
            surface[n*24+ 0] = x + 0; surface[n*24+ 1] = y + 0; surface[n*24+ 2] = z + 0; surface[n*24+ 3] = w + 0;
            surface[n*24+ 4] = x + 0; surface[n*24+ 5] = y + 1; surface[n*24+ 6] = z + 0; surface[n*24+ 7] = w + 0;
            surface[n*24+ 8] = x + 1; surface[n*24+ 9] = y + 1; surface[n*24+10] = z + 0; surface[n*24+11] = w + 0;
            surface[n*24+12] = x + 1; surface[n*24+13] = y + 1; surface[n*24+14] = z + 0; surface[n*24+15] = w + 0;
            surface[n*24+16] = x + 1; surface[n*24+17] = y + 0; surface[n*24+18] = z + 0; surface[n*24+19] = w + 0;
            surface[n*24+20] = x + 0; surface[n*24+21] = y + 0; surface[n*24+22] = z + 0; surface[n*24+23] = w + 0;
            break;
    }
}

int project(int x,int y,int z) {
    return x + 16 * y + 16 * 16 * z;
}

//int* unproject(xyz) {
//    x = xyz % 16
//    y = (xyz / 16) % 16
//    z = (xyz / 16 / 16)
//    return int[3] {x,y,z};
//}

char* extractSurface(int* len, int chunk[16*16*16]) {
    char allFilled = 1;

    for (int x = 0; x < 16; x++) {
        for (int y = 0; y < 16; y++) {
            for (int z = 0; z < 16; z++) {
                if (chunk[project(x,y,z)] != chunk[0]) {
                    allFilled = 0;
                    x=16;y=16;z=16;break;
                }
            }
        }
    }

    if (allFilled) {
        *len = 0;
        return malloc(0);
    }

    int i = 0;
    char* surface = (char*) malloc(50000 * sizeof(char) * ++i);
    int n = 0;

    
    for (int x = 0; x < 16; x++) {
        for (int y = 0; y < 16; y++) {
            for (int z = 0; z < 16; z++) {
                if (chunk[project(x,y,z)]) {
                    if (x+1 < 16 && !chunk[project(x+1,y,z)]) {
                        appendFace(n++,EAST,surface,x,y,z,1);
                    }
                    if (x-1 >= 0 && !chunk[project(x-1,y,z)]) {
                        appendFace(n++,WEST,surface,x,y,z,1);
                    }
                    if (y+1 < 16 && !chunk[project(x,y+1,z)]) {
                        appendFace(n++,TOP,surface,x,y,z,0);
                    }
                    if (y-1 >= 0 && !chunk[project(x,y-1,z)]) {
                        appendFace(n++,BOTTOM,surface,x,y,z,0);
                    }
                    if (z+1 < 16 && !chunk[project(x,y,z+1)]) {
                        appendFace(n++,NORTH,surface,x,y,z,1);
                    }
                    if (z-1 >= 0 && !chunk[project(x,y,z-1)]) {
                        appendFace(n++,SOUTH,surface,x,y,z,1);
                    }
                    if (n * 24 > i * 50000 - 24 * 6) {
                        surface = realloc(surface, 50000 * ++i * sizeof(char));
                    }
                }
            }
        }
    }

    *len = n * 24 * sizeof(char);
    surface = realloc(surface,*len);

    return surface;
}

#ifdef WITHMAIN
int main () {

    int chunk[16*16*16];

    // generate random chunk
    for (int x = 0; x < 16; x++) {
        for (int y = 0; y < 16; y++) {
            for (int z = 0; z < 16; z++) {
                chunk[project(x,y,z)] = rand() & 1;
            }
        }
    }

    // start timer
    clock_t start = clock(), diff;

    int len = 0;

    // extract the surface
    extractSurface(&len,chunk);
    
    // collect time difference
    diff = clock() - start;
    int us = diff * 1000000 / CLOCKS_PER_SEC;
    printf("Time taken %d microseconds\n", us);

    return 0;
}
#endif
