import "std/modules/map.goose"
module HTTP 
  declare fetch(url: string, headers: string): string

  def get(url: string): string do
    return HTTP::fetch(url, "")
  end
end