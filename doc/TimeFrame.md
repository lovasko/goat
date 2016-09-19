# TimeFrame
The `TimeFrame` type is used to represent a finite block of time that
contains a set of time points which are stored in a memory-efficient
manner. The following sections discuss the design of the API, the type
definition and the compression algorithm that is used.

## API
There are three functions provided on top of the `TimeFrame` type:
 * `timeEncode :: [Word32]  -> TimeFrame`
 * `timeDecode :: TimeFrame -> [Word32]`
 * `timeHead   :: TimeFrame -> Maybe Word32`

The `timeEncode` function can be used to encode a list of 32-bit words
into a compressed/succinct frame form. The compression algorithm is
described in sections below. It expects the time points to be in a descending
order.

The `timeDecode` function can be used to decode a compressed/succinct
frame form into a list of raw 32-bit words. It can be viewed as a counterpart
to the `timeEncode` function - in fact when composed, these two functions
behave as the `id`entity function.

The `timeHead` function yields the first time data point - if there is any -
stored in the frame form. It uses the `Maybe` monad to express failure.

## Type
The `TimeFrame` type is defined as follows:

```haskell
data TimeFrame = TimeFrame
                 (Maybe Word32) -- ^ first time point
                 (Maybe Word32) -- ^ second time point
                 Int            -- ^ number of valid bits
                 B.ByteString   -- ^ bits
```

Due to the way how the compression algorithm works, it is necessary to store
the first two time points separately; yet it is possible that a set of time
points that needs to be encoded will have only one or even zero entries. In
such case, the `Maybe` monad is used to express this situation. In case that
the first or second field of type is `Nothing` it is not necessary to decode
the rest of the fields.

The last two fields are used to store the results of the compression algorithm
in the most efficient way - almost as a C byte array.

### Class instances
Currently, `TimeFrame` implements these typeclasses:
 * `Eq`
 * `GoatSwim.Frame`

## Compression algorithm
The compression algorithm makes certain assumptions about the time data points
that are inserted into the `TimeFrame` type:
 * all values are distinct
 * when requested to encode a list, elements must be in descending order
 * values are fairly regular (e.g. 5 second interval)

TBD

