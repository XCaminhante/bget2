//@+leo-ver=5-thin
//@+node:caminhante.20221211153038.1: * @file bget.c
//@@tabwidth -2
//@+others
//@+node:caminhante.20221211132331.1: ** /includes
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//@+node:caminhante.20221211153050.1: ** /defines
typedef ssize_t bufsize;
#define SizeQuant 8
#define BestFit 1
// 16 KiB
#define exp_incr (4096*4)
#define FreeWipeCONST 0x55
//@+node:caminhante.20221211153115.1: ** /structs and defines
//@+node:caminhante.20221211153119.1: *3* Queue links, qlinks
struct qlinks {
  struct bfhead *flink; /* Forward link */
  struct bfhead *blink; /* Backward link */
};
//@+node:caminhante.20221211153159.1: *3* Header in allocated and free buffers, bhead
struct bhead {
  bufsize prevfree; /* Relative link back to previous
                      free buffer in memory or 0 if
                      previous buffer is allocated.  */
  bufsize bsize;    /* Buffer size: positive if free,
                      negative if allocated. */
};
#define BH(p)   ((struct bhead *) (p))
//@+node:caminhante.20221211153307.1: *3* Header in directly allocated buffers, bdhead
struct bdhead {
  bufsize tsize;    /* Total size, including overhead */
  struct bhead bh;  /* Common header */
};
#define BDH(p)  ((struct bdhead *) (p))
//@+node:caminhante.20221211153349.1: *3* Header in free buffers, bfhead
struct bfhead {
  struct bhead bh;  /* Common allocated/free header */
  struct qlinks ql; /* Links on free list */
};
#define BFH(p)  ((struct bfhead *) (p))
//@+node:caminhante.20221211155028.1: *3* Minimum allocation quantum
#define QLSize  (sizeof(struct qlinks))
#define SizeQ   (bufsize)((SizeQuant > QLSize) ? SizeQuant : QLSize)
//@+node:caminhante.20221211155114.1: *3* End sentinel
/* End sentinel: value placed in bsize field of dummy block delimiting end of pool block. The most
  negative number which will fit in a bufsize, defined in a way that the compiler will accept. */
#define ESent   ((bufsize) (-(((1L << (sizeof(bufsize) * 8 - 2)) - 1) * 2) - 2))
//@+node:caminhante.20221211162017.1: *3* Automatic expansion block management functions
static size_t (*compfcn) (struct bfhead *pool, bufsize sizereq, int sequence) = NULL;
static void* (*acqfcn) (bufsize size) = NULL;
static void (*relfcn) (void *buf) = NULL;
//@+node:caminhante.20221211153451.1: ** /functions
//@+node:caminhante.20221211153455.1: *3* void bnew (struct bfhead *pool)
void bnew (struct bfhead *pool) {
  *pool = (struct bfhead){ .bh={0,0}, .ql={pool,pool} };
}
//@+node:caminhante.20221211162125.1: *3* void bpool (pool, buf, len)
void bpool (struct bfhead *pool, void *buf, bufsize len) {
  struct bfhead *b = BFH(buf);
  struct bhead *bn;
  len &= ~(SizeQuant - 1);
  /* Since the block is initially occupied by a single free buffer, it had better not be (much)
    larger than the largest buffer whose size we can store in bhead.bsize. */
  assert(len - sizeof(struct bhead) <= -((bufsize) ESent + 1));
  /* Clear the backpointer at the start of the block to indicate that there is no free block prior
    to this one. That blocks recombination when the first block in memory is released. */
  b->bh.prevfree = 0;
  /* Chain the new block to the free list. */
  assert(pool->ql.blink->ql.flink == pool);
  assert(pool->ql.flink->ql.blink == pool);
  b->ql.flink = pool;
  b->ql.blink = pool->ql.blink;
  pool->ql.blink = b;
  b->ql.blink->ql.flink = b;
  /* Create a dummy allocated buffer at the end of the pool. This dummy buffer is seen when a buffer
    at the end of the pool is released and blocks recombination of the last buffer with the dummy
    buffer at the end. The length in the dummy buffer is set to the largest negative number to
    denote the end of the pool for diagnostic routines (this specific value is not counted on by the
    actual allocation and release functions). */
  len -= sizeof(struct bhead);
  b->bh.bsize = (bufsize) len;
  (void) memset(((char *) b) + sizeof(struct bfhead), FreeWipeCONST, (len - sizeof(struct bfhead)));
  bn = BH(((char *) b) + len);
  bn->prevfree = (bufsize) len;
  /* Definition of ESent assumes two's complement! */
  assert((~0) == -1);
  bn->bsize = ESent;
}
//@+node:caminhante.20221211212258.1: *3* bool bpoolv (pool)
// Validate a buffer pool
/*
bool bpoolv (struct bfhead *pool) {
  struct bfhead *b = BFH((void*) pool);
  bufsize bs = b->bh.bsize;
  if (bs != 0) { puts("That's not the buffer pool head"); return false; }
  printf("BUFFER POOL %p:\n", (void*)pool);
  b = b->ql.flink;
  do {
    struct bfhead *bn = b->ql.flink;
    printf("BFHEAD %p:\n", (void*)b);
    while (b->bh.bsize != ESent && b->bh.bsize != 0) {
      bs = b->bh.bsize;
      printf(" BHEAD %p (%ld bytes)\n", (void*)b, bs);
      if (bs < 0) {
        bs = -bs;
      } else {
        char *lerr = "";
        if ((b->ql.blink->ql.flink != b) || (b->ql.flink->ql.blink != b)) {
          printf("Free block: size %6ld bytes.  (Bad free list links)\n", (long) bs);
          return false;
        }
        lerr = ((char *) b) + sizeof(struct bfhead);
        if ((bs > (bufsize)sizeof(struct bfhead)) && ((*lerr != FreeWipeCONST) ||
        (memcmp(lerr, lerr + 1, (bs - (sizeof(struct bfhead) + 1))) != 0))) {
          printf("(Contents of above free block have been overstored.)\n");
          return false;
        }
      }
      b = BFH(((char *) b) + bs);
    }
    b = bn;
  } while (b != pool);
  return true;
}
*/
//@+node:caminhante.20221211155317.1: *3* void* bget (pool, requested_size)
void* bget (struct bfhead *pool, size_t requested_size) {
  //@+others
  //@+node:caminhante.20221211164649.1: *4* preparation
  bufsize size = requested_size;
  struct bfhead *b;
  #ifdef BestFit
  struct bfhead *best;
  #endif
  void *buf;
  int compactseq = 0;
  assert(size > 0);
  if (size < SizeQ) { /* Need at least room for the */
    size = SizeQ;     /* queue links.  */
  }
  size = (size + (SizeQuant - 1)) & (~(SizeQuant - 1));
  size += sizeof(struct bhead); /* Add overhead in allocated buffer
                                   to size required. */
  //@+node:caminhante.20221211164747.1: *4* intra pool acquisition
  /* Wrap a loop around the allocation process to allow compaction to intervene in case we don't
    find a suitable buffer in the chain. */
  while (1) {
    b = pool->ql.flink;
    #ifdef BestFit
    best = pool;
    #endif
    /* Scan the free list searching for the first buffer big enough
      to hold the requested size buffer. */
    #ifdef BestFit
    while (b != pool) {
      if (b->bh.bsize >= size) {
        if ((best == pool) || (b->bh.bsize < best->bh.bsize)) { best = b; } }
      b = b->ql.flink;  /* Link to next buffer */
    }
    b = best;
    #endif
    while (b != pool) {
      if ((bufsize) b->bh.bsize >= size) {
        /* Buffer is big enough to satisfy the request. Allocate it to the caller. We must decide
          whether the buffer is large enough to split into the part given to the caller and a free
          buffer that remains on the free list, or whether the entire buffer should be removed from
          the free list and given to the caller in its entirety. We only split the buffer if enough
          room remains for a header plus the minimum quantum of allocation. */
        if ((b->bh.bsize - size) > (SizeQ + (bufsize)(sizeof(struct bhead)))) {
          struct bhead *ba, *bn;
          ba = BH(((char *) b) + (b->bh.bsize - size));
          bn = BH(((char *) ba) + size); // == ba from the next branch
          assert(bn->prevfree == b->bh.bsize);
          /* Subtract size from length of free block. */
          b->bh.bsize -= size;
          /* Link allocated buffer to the previous free buffer. */
          ba->prevfree = b->bh.bsize;
          /* Plug negative size into user buffer. */
          ba->bsize = -(bufsize) size;
          /* Mark buffer after this one not preceded by free block. */
          bn->prevfree = 0;
          buf = (void *) ((((char *) ba) + sizeof(struct bhead)));
          return buf;
        } else {
          struct bhead *ba;
          ba = BH(((char *) b) + b->bh.bsize);
          assert(ba->prevfree == b->bh.bsize);
          /* The buffer isn't big enough to split. Give the whole shebang to the caller and remove
            it from the free list. */
          assert(b->ql.blink->ql.flink == b);
          assert(b->ql.flink->ql.blink == b);
          b->ql.blink->ql.flink = b->ql.flink;
          b->ql.flink->ql.blink = b->ql.blink;
          /* Negate size to mark buffer allocated. */
          b->bh.bsize = -(b->bh.bsize);
          /* Zero the back pointer in the next buffer in memory to indicate that this buffer is
            allocated. */
          ba->prevfree = 0;
          /* Give user buffer starting at queue links. */
          buf =  (void *) &(b->ql);
          return buf;
        }
      }
      b = b->ql.flink;  /* Link to next buffer */
    }
    /* We failed to find a buffer. If there's a compact function defined, notify it of the size
      requested. If it returns TRUE, try the allocation again. */
    if ((compfcn == NULL) || (!(*compfcn)(pool, size, ++compactseq))) {
      break;
    }
  }
  //@+node:caminhante.20221211165052.1: *4* pool expansion
  /* No buffer available with requested size free. */
  /* Don't give up yet -- look in the reserve supply. */
  if (acqfcn != NULL) {
    if (size > exp_incr - (bufsize)sizeof(struct bhead)) {
      /* Request is too large to fit in a single expansion block. Try to satisy it by a direct
        buffer acquisition. */
      struct bdhead *bdh;
      size += sizeof(struct bdhead) - sizeof(struct bhead);
      if ((bdh = BDH((*acqfcn)((bufsize) size))) != NULL) {
        /* Mark the buffer special by setting the size field of its header to zero. */
        bdh->bh.bsize = 0;
        bdh->bh.prevfree = 0;
        bdh->tsize = size;
        buf =  (void *) (bdh + 1);
        return buf;
      }
    } else {
      /* Try to obtain a new expansion block */
      void *newpool;
      if ((newpool = (*acqfcn)((bufsize) exp_incr)) != NULL) {
        bpool(pool, newpool, exp_incr);
        buf = bget(pool, requested_size); /* This can't, I say, can't get into a loop. */
        return buf;
      }
    }
  }
  //@-others
  /*  Still no buffer available */
  return NULL;
}
//@+node:caminhante.20221211165726.1: *3* void* bgetz (pool, requested_size)
void* bgetz (struct bfhead *pool, bufsize requested_size) {
  char *buf = (char *) bget(pool, requested_size);
  if (buf != NULL) {
    struct bhead *b;
    bufsize rsize;
    b = BH(buf - sizeof(struct bhead));
    rsize = -(b->bsize);
    if (rsize == 0) {
      struct bdhead *bd;
      bd = BDH(buf - sizeof(struct bdhead));
      rsize = bd->tsize - sizeof(struct bdhead);
    } else {
      rsize -= sizeof(struct bhead);
    }
    assert(rsize >= requested_size);
    (void) memset(buf, 0, rsize);
  }
  return ((void *) buf);
}
//@+node:caminhante.20221211170141.1: *3* void brel (pool, buf)
void brel (struct bfhead *pool, void *buf) {
  struct bfhead *b, *bn;
  b = BFH(((char *) buf) - sizeof(struct bhead));
  assert(buf != NULL);
  if (b->bh.bsize == 0) { /* Directly-acquired buffer? */
    struct bdhead *bdh;
    bdh = BDH(((char *) buf) - sizeof(struct bdhead));
    assert(b->bh.prevfree == 0);
    (void) memset((char *) buf, FreeWipeCONST, (bdh->tsize - sizeof(struct bdhead)));
    assert(relfcn != NULL);
    (*relfcn)((void *) bdh);  /* Release it directly. */
    return;
  }
  /* Buffer size must be negative, indicating that the buffer is allocated. */
  if (b->bh.bsize >= 0) {
    bn = NULL;
  }
  assert(b->bh.bsize < 0);
  /* Back pointer in next buffer must be zero, indicating the same thing: */
  assert(BH((char *) b - b->bh.bsize)->prevfree == 0);
  /* If the back link is nonzero, the previous buffer is free.  */
  if (b->bh.prevfree != 0) {
    /* The previous buffer is free. Consolidate this buffer with it by adding the length of this
      buffer to the previous free buffer. Note that we subtract the size in the buffer being
      released, since it's negative to indicate that the buffer is allocated. */
    bufsize size = b->bh.bsize;
    /* Make the previous buffer the one we're working on. */
    assert(BH((char *) b - b->bh.prevfree)->bsize == b->bh.prevfree);
    b = BFH(((char *) b) - b->bh.prevfree);
    b->bh.bsize -= size;
  } else {
    /* The previous buffer is allocated. Insert this buffer on the free list as an isolated free
      block. */
    assert(pool->ql.blink->ql.flink == pool);
    assert(pool->ql.flink->ql.blink == pool);
    b->ql.flink = pool;
    b->ql.blink = pool->ql.blink;
    pool->ql.blink = b;
    b->ql.blink->ql.flink = b;
    b->bh.bsize = -b->bh.bsize;
  }
  /* Now we look at the next buffer in memory, located by advancing from the start of this buffer by
    its size, to see if that buffer is free. If it is, we combine this buffer with the next one in
    memory, dechaining the second buffer from the free list. */
  bn =  BFH(((char *) b) + b->bh.bsize);
  if (bn->bh.bsize > 0) {
    /* The buffer is free. Remove it from the free list and add its size to that of our buffer. */
    assert(BH((char *) bn + bn->bh.bsize)->prevfree == bn->bh.bsize);
    assert(bn->ql.blink->ql.flink == bn);
    assert(bn->ql.flink->ql.blink == bn);
    bn->ql.blink->ql.flink = bn->ql.flink;
    bn->ql.flink->ql.blink = bn->ql.blink;
    b->bh.bsize += bn->bh.bsize;
    /* Finally, advance to the buffer that follows the newly consolidated free block. We must set
      its backpointer to the head of the consolidated free block. We know the next block must be an
      allocated block because the process of recombination guarantees that two free blocks will
      never be contiguous in memory. */
    bn = BFH(((char *) b) + b->bh.bsize);
  }
  (void) memset(((char *) b) + sizeof(struct bfhead), FreeWipeCONST, (b->bh.bsize - sizeof(struct bfhead)));
  assert(bn->bh.bsize < 0);
  /* The next buffer is allocated. Set the backpointer in it to point to this buffer; the previous
    free buffer in memory. */
  bn->bh.prevfree = b->bh.bsize;
  /* If a block-release function is defined, and this free buffer constitutes the entire block,
    release it. */
  if (relfcn != NULL && bn->bh.bsize == ESent && b->bh.bsize == exp_incr - sizeof(struct bhead)) {
    assert(b->bh.prevfree == 0);
    /*  Unlink the buffer from the free list  */
    b->ql.blink->ql.flink = b->ql.flink;
    b->ql.flink->ql.blink = b->ql.blink;
    (*relfcn)(b);
  }
}
//@+node:caminhante.20221211210846.1: ** test acquire/release functions
/*
//@+others
//@+node:caminhante.20221211210857.1: *3* void* facq (bufsize size)
void* facq (bufsize size) {
  void* b = malloc(size);
  printf("allocating %p, %ld bytes\n",b,size);
  return b;
}
//@+node:caminhante.20221211210912.1: *3* void frel (void *buf)
void frel (void *buf) {
  printf("freeing %p\n",buf);
  free(buf);
}
//@+node:caminhante.20221211154202.1: *3* int test ()
int test () {
  struct bfhead p; bnew(&p);
  acqfcn = facq;
  relfcn = frel;
  void *a[10];
  for (long b=0; b<10; b++) { a[b] = bget(&p, 100); printf("alloc %ld -> %p\n", b+1, a[b]); }
  for (long b=5; b>=3; b--) { brel(&p, a[b]); printf("free %ld -> %p\n", b+1, a[b]); }
  printf("Buffer pool: %s\n", ( bpoolv(&p) ? "OK" : "PROBLEM" ) );
  brel(&p, a[9]);
  return 0;
}
//@-others
*/
//@-others
//@-leo
