defmodule Discovery do
  @moduledoc """
  This is currently just a stub that calls start methods on Erlang modules
  :ssdp and :ssdp_root_device
  """

  @default_usn Application.get_env :discovery, :usn, "urn:cellulose-io:service:cell:1"
  @default_port Application.get_env :discovery, :port, 80
  @default_uri Application.get_env :discovery, :uri, "/jrtp/"

  @doc "Start SSDP and SSDP Root Device erlang modules"
  def start(args \\ []), do: :ssdp.start_all(merge_args(args))

  @doc """
  Starts SSDP and SSDP Root Device erlang modules with link to celling process
  """
  def start_link(args \\ []) do
    :ssdp_root_device.start_link(merge_args(args))
    :ssdp.start_link(merge_args(args))
  end

  @doc "Start SSDP erlang module"
  def start_ssdp(args \\ []), do: :ssdp.start(merge_args(args))

  @doc "Start SSDP Root device erlang module"
  def start_root_device(args \\ []), do: :ssdp_root_device.start(merge_args(args))

  # Merges the passed arguments with the defaults
  defp merge_args(args) do
    usn = Dict.get(args, :usn, @default_usn)
    port = Dict.get(args, :port, @default_port)
    uri = Dict.get(args, :uri, @default_uri)
    [usn: usn, port: port, uri: uri]
  end
end
