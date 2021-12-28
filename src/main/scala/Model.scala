package com.pako.wav


case class WavHeader (
    riff: String,
    size: Int
)

/*
case class WavHeader(
    riff : Seq[Byte],
    size : Seq[Byte],
    descrip : Seq[Byte],
    fmt : Seq[Byte],
    fmtSize : Seq[Byte],
    format : Seq[Byte],
    channels : Seq[Byte],
    sampleRate : Seq[Byte],
    byteRate : Seq[Byte],
    blockAlign : Seq[Byte],
    bitsPerSample : Seq[Byte],
    dataDescription : Seq[Byte],
    dataSize : Seq[Byte],
    data: Seq[Byte]
)
*/

