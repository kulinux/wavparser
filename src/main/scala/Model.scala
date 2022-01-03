package com.pako.wav

case class WavHeader (
    riff: String,
    size: Int,
    wave: String,
    fmt: String,
    fmtSize: Int,
    audioFormat: Int,
    numChannels: Int
)

object WavHeader {
    //Audio Types
    val PCM = 1

    //Num Channels
    val Mono = 1
    val Stereo = 2

    def empty() = WavHeader("", -1, "", "", -1, -1, -1)
}

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

