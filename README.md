# crawly
### Ben McGunigle <bnmcgn@gmail.com>

Crawly is a tool for searching and fetching archived web pages from Common Crawl and Internet Archive. It includes a rudimentary WARC parser for handling Common Crawl results.

## Status

Crawly should be regarded as a sketch. The pieces work, but aren't well integrated. The API will change. For now, use it as a starting point.

## Usage

## url-search
#### Function: search &key (limit 1000) (source :common-crawl)

Search the archive for url matches. Looks in Common Crawl by default. Set :source to :internet-archive to search archive.org

Returns a list of captures.

## get-archive-from-capture
#### Function: source capture

Retrieve one of the results from the `url-search` function above. The first parameter must be one of :common-crawl or :internet-archive, matching the source of the search results.


## read-warc
#### Function: stream

Loads the WARC supplied in `stream`.

Returns a list that contains a header and a content section for each item in the WARC. The header will be a hash table, the content section will be an array of octets.

## first-matching-record
#### Function: stream-or-path predcate &key (return-type :content)

Steps through the records in a WARC, passing the headers of each to the predicate as a hash table. On success the record will be treated according to :return-type. If it is :content (default), the record body will be returned as an array of octets. :length will cause it to return the length of the content with the stream queued up to read. :consume will return T with the stream moved to the end of the content.

## get-record-for-url
#### Function: stream-or-path url

Finds the first response record in the supplied WARC containing the result of `url`. Returns the page as a string.

FIXME: does no response checking. Not going to work so hot on binary files.

## License

Apache 2.0

