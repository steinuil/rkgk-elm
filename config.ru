require 'rest-client'

use Rack::Static, root: 'static', urls: {
  '/' => 'pixiv.html',
  '/pixiv.js' => 'pixiv.js',
  '/pixiv.css' => 'pixiv.css',
  '/logo.png' => 'logo.png',
}

app = lambda do |env|
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
    unless (path = env['REQUEST_PATH']) =~ /\/https:\/\/.*/
      return [ 404, {}, [] ]
    end

    # Let's just assume Pixiv's image urls never change
    if env['HTTP_IF_MODIFIED_SINCE']
      return [ 304, {}, [] ]
    end

    req_url = path[1..-1] + '?' + env['QUERY_STRING']

    orig_headers = env.select do |h, v|
      h.start_with? 'HTTP_'
    end.map do |h, v|
      [ h.sub(/^HTTP_/, ''), v ]
    end

    req_headers = {
      'Referer' => 'https://pixiv.net',
      'App-OS' => 'android',
      'App-OS-Version' => '6.0.1',
      'App-Version' => '5.0.56',
      'User-Agent' => 'PixivAndroidApp/5.0.56 (Android 6.0.1; SM-G850F)'
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
          RestClient.post req_url, req_body, req_headers
        end
      rescue RestClient::ExceptionWithResponse => e
        e.response
      rescue RestClient::ServerBrokeConnection, OpenSSL::SSL::SSLError, Errno::ECONNRESET
        retry
      end

    content_type = response.headers[:content_type]

    headers = {
      'Content-Type' => content_type,
      'Access-Control-Allow-Origin' => '*'
    }

    if content_type =~ /image\/.+/
      headers['Last-Modified'] = Time.now.strftime('%a, %d %b %Y %T %Z')
    end

    [ response.code.to_s, headers, [ response.body ] ]

  else
    [ 405, {}, [] ]
  end
end

run app
