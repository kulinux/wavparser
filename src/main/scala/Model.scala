package com.pako.wav


case class FmtExtraParam (size: Int, extraParam: Array[Byte]) 

case class FmtHeader(
    fmt: String,
    fmtSize: Int,
    audioFormat: WavHeader.AudioFormat,
    numChannels: WavHeader.NumChannel,
    sampleRate: Int,
    byteRate: Int,
    blockAlign: Int,
    bitsPerSample: Int,
    extraParam: Option[FmtExtraParam])

object FmtHeader {
    def empty() = FmtHeader("", -1, -1, -1, -1, -1, -1, -1, Option.empty[FmtExtraParam])
}

case class DataHeader(data: Array[Byte])

object DataHeader {
    def empty() = DataHeader(Array[Byte]())
}

case class UnknownHeader(header: String, data: Array[Byte])

case class WavHeader (
    riff: String,
    size: Int,
    wave: String,
    fmt: FmtHeader,
    data: DataHeader,
    unknownHeader: Seq[UnknownHeader] = Seq()
)

object WavHeader {

    //Audio Types
    type AudioFormat = Int
    val PCM: AudioFormat = 1

    //Num Channels
    type NumChannel = Int
    val Mono: NumChannel = 1
    val Stereo: NumChannel = 2
    
    def empty() = WavHeader("", -1, "", FmtHeader.empty(), DataHeader.empty())
}


