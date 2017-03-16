require 'rest-client'

use Rack::Static, urls: [ '' ], root: 'static', index: 'index.html'

app = Proc.new do |env|
  case method = env['REQUEST_METHOD']
  when 'OPTIONS'
    headers = {
      'Access-Control-Allow-Origin' => '*',
      'Access-Control-Allow-Headers' => [
          'App-OS', 'App-OS-Version', 'App-Version',
          'User-Agent', 'Authorization', 'Content-Type'
        ].join(', '),
      'Access-Control-Allow-Methods' => '*'
    }

    [ 200, headers, [] ]

  when 'GET', 'POST'
    unless path = env['REQUEST_PATH'] ~= /\/https:\/\/.*/
      return [ 404, {}, [] ]
    end

    req_url = path[1..-1] + '?' + env['QUERY_STRING']

    req_headers = {
      'Referer' => 'https://pixiv.net',
      'App-OS' => 'ios',
      'App-OS-Version' => '10.2.1',
      'App-Version' => '6.4.0',
      'User-Agent' => 'PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)'
    }

    if auth = env['HTTP_AUTHORIZATION']
      req_headers.merge!({
        'Authorization' => auth
      })
    end

    req_body = Rack::Request.new(env).params

    response =
      begin
        if method == 'GET'
          RestClient.get req_url, req_headers
        else
          RestClient.post req_url, req_headers, req_body
        end
      rescue RestClient::ExceptionWithResponse => e
        e.response
      end

    headers = {
      'Content-Type' => response.headers[:content_type],
      'Access-Control-Allow-Origin' => '*'
    }

    [ response.code.to_s, headers, [ response.body ] ]

  else
    [ 405, {}, [] ]
  end
end

run app
