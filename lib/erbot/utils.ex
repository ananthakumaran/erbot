defmodule Erbot.Utils do

  def gist(text) do
    url = "https://api.github.com/gists"
    {:ok, body} = JSEX.encode([public: true, files: ["output.txt": [content: text]]])
    HTTPotion.Response[status_code: 201, body: response_body] = HTTPotion.post(url, body, [{"User-Agent", "Erbot"}])
    {:ok, response} = JSEX.decode(response_body)
    {_, [{"output.txt", file_attributes}]} = :lists.keyfind("files", 1, response)
    {_, gist_url} = :lists.keyfind("raw_url", 1, file_attributes)
    gist_url
  end


  def triple_to_timestamp({mega_secs, secs, micro_secs}) do
    mega_secs * 1000000000000 + secs * 1000000 + micro_secs
  end

  def timestamp_to_triple(t) do
    {div(t, 1000000000000), rem(div(t, 1000000), 1000000), rem(t, 1000000)}
  end
end
