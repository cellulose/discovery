defmodule Discovery do
  @moduledoc """
  This is currently just a stub that calls start methods on Erlang modules
  :ssdp and :ssdp_root_device
  """

  @default_usn Application.get_env :discovery, :usn, "urn:cellulose-io:service:cell:1"
  @default_port Application.get_env :discovery, :port, 80
  @default_uri Application.get_env :discovery, :uri, "/jrtp/"

  def start(args \\ []), do: :ssdp.start(merge_args(args))

  def start_all(args \\ []), do: :ssdp.start_all(merge_args(args))

  def start_root(args \\ []), do: :ssdp_root_device.start(merge_args(args))

  defp merge_args(args) do
    usn = Dict.get(args, :usn, @default_usn)
    port = Dict.get(args, :port, @default_port)
    uri = Dict.get(args, :uri, @default_uri)
    [usn: usn, port: port, uri: uri]
  end
end
