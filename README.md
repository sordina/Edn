
# Edn

A simple utility to pretty-print [Edn](https://github.com/edn-format/edn) formatted data.

Edn parsing library used and modified from [hedn](https://bitbucket.org/dpwiz/hedn).

## Usage

		Usage:
			edn FILE
			edn < FILE

## Example

		$ echo '{:foo {:key :value :attributes [:x :y :z] \\ 
						 :fuzzes [{:i :love} {:red :pandas} {:for :real}]}}' | edn
		{
			:foo {
				:attributes [:x :y :z]
				:fuzzes [{
					:i :love
				} {
					:red :pandas
				} {
					:for :real
				}]
				:key :value
			}
		}

## Binaries

<http://sordina.binaries.s3.amazonaws.com/edn-0.1.0.0-MacOSX-10.9.2-13C64.zip>
