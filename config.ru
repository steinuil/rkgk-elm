require 'rest-client'

app = proc do |env|
  method = env['REQUEST_METHOD']

  case method
  when 'OPTIONS'
    allowed_headers =
      [ 'App-OS', 'App-OS-Version', 'App-Version', 'User-Agent', 'Authorization', 'Content-Type' ]

    headers = {
      'Access-Control-Allow-Origin' => '*',
      'Access-Control-Allow-Headers' => allowed_headers.join(', '),
      'Access-Control-Allow-Methods' => '*'
    }

    [ '200', headers, [] ]

  when 'GET', 'POST'
    case env['REQUEST_PATH']
    when '/'
      [ '200', {}, [ File.read('pixiv.html') ] ]
    when '/pixiv.css'
      [ '200', {}, [ File.read('pixiv.css') ] ]
    when '/pixiv.js'
      [ '200', {}, [ File.read('pixiv.js') ] ]
    when /\/https:\/\/.*/
      url = env['REQUEST_PATH'][1..-1] + '?' + env['QUERY_STRING']

      h = {
        'Referer' => 'https://pixiv.net',
        'App-OS' => 'ios',
        'App-OS-Version' => '10.2.1',
        'App-Version' => '6.4.0',
        'User-Agent' => 'PixivIOSApp/6.0.9 (iOS 10.2.1; iPhone8,1)'
      }

      body = Rack::Request.new(env).params

      res =
        begin
          if method == 'GET'
            RestClient.get url, h
          else
            RestClient.post url, body, h
          end
        rescue RestClient::ExceptionWithResponse => e
          e.response
        end

      headers = {
        'Content-Type' => res.headers[:content_type],
        'Access-Control-Allow-Origin' => '*'
      }

      [ res.code.to_s, headers, [ res.body ] ]
    else
      [ '404', {}, [] ]
    end
  end
end

run app
