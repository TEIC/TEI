<?xml version="1.0"?>
<!--
  Copyright 1999-2004 The Apache Software Foundation

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

<!-- CVS $Id$ -->

<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:dir="http://apache.org/cocoon/directory/2.0">

  <xsl:template match="/">
    <html>
      <head>
        <title><xsl:value-of select="dir:directory/@name"/></title>
        <style>
          <xsl:comment>
            body { background-color: #ffffff }
          </xsl:comment>
        </style>
      </head>
      <body>
        <h1>Directory Listing of <xsl:value-of select="dir:directory/@name"/></h1>
        <table border="0">
          <tr>
            <td><a href="../"><i>parent directory</i></a></td>
          </tr>
          <tr>
            <td>&#160;</td>
          </tr>
          <xsl:apply-templates/>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="dir:directory/dir:directory">
    <tr>
      <td><a href="{@name}/"><i><xsl:value-of select="@name"/></i></a></td>
      <td><xsl:value-of select="@date"/></td>
    </tr>
  </xsl:template>

  <xsl:template match="dir:file">
    <tr>
      <td><a href="{@name}"><xsl:value-of select="@name"/></a></td>
      <td><xsl:value-of select="@date"/></td>
    </tr>
  </xsl:template>

</xsl:stylesheet>
