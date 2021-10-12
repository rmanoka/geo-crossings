(function() {var implementors = {};
implementors["geo_types"] = [{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a> + <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Coordinate.html\" title=\"struct geo_types::Coordinate\">Coordinate</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Coordinate.html\" title=\"struct geo_types::Coordinate\">Coordinate</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::coordinate::Coordinate"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Point.html\" title=\"struct geo_types::Point\">Point</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Point.html\" title=\"struct geo_types::Point\">Point</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::point::Point"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.MultiPoint.html\" title=\"struct geo_types::MultiPoint\">MultiPoint</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.MultiPoint.html\" title=\"struct geo_types::MultiPoint\">MultiPoint</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::multi_point::MultiPoint"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Line.html\" title=\"struct geo_types::Line\">Line</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Line.html\" title=\"struct geo_types::Line\">Line</a>&lt;T&gt;","synthetic":false,"types":["geo_types::line::Line"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.LineString.html\" title=\"struct geo_types::LineString\">LineString</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.LineString.html\" title=\"struct geo_types::LineString\">LineString</a>&lt;T&gt;","synthetic":false,"types":["geo_types::line_string::LineString"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.MultiLineString.html\" title=\"struct geo_types::MultiLineString\">MultiLineString</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.MultiLineString.html\" title=\"struct geo_types::MultiLineString\">MultiLineString</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::multi_line_string::MultiLineString"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Polygon.html\" title=\"struct geo_types::Polygon\">Polygon</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Polygon.html\" title=\"struct geo_types::Polygon\">Polygon</a>&lt;T&gt;","synthetic":false,"types":["geo_types::polygon::Polygon"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.MultiPolygon.html\" title=\"struct geo_types::MultiPolygon\">MultiPolygon</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.MultiPolygon.html\" title=\"struct geo_types::MultiPolygon\">MultiPolygon</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::multi_polygon::MultiPolygon"]},{"text":"impl&lt;T:&nbsp;<a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"enum\" href=\"geo_types/enum.Geometry.html\" title=\"enum geo_types::Geometry\">Geometry</a>&lt;T&gt;&gt; for <a class=\"enum\" href=\"geo_types/enum.Geometry.html\" title=\"enum geo_types::Geometry\">Geometry</a>&lt;T&gt;","synthetic":false,"types":["geo_types::geometry::Geometry"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.GeometryCollection.html\" title=\"struct geo_types::GeometryCollection\">GeometryCollection</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.GeometryCollection.html\" title=\"struct geo_types::GeometryCollection\">GeometryCollection</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::geometry_collection::GeometryCollection"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Triangle.html\" title=\"struct geo_types::Triangle\">Triangle</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Triangle.html\" title=\"struct geo_types::Triangle\">Triangle</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::triangle::Triangle"]},{"text":"impl&lt;T&gt; <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;<a class=\"struct\" href=\"geo_types/struct.Rect.html\" title=\"struct geo_types::Rect\">Rect</a>&lt;T&gt;&gt; for <a class=\"struct\" href=\"geo_types/struct.Rect.html\" title=\"struct geo_types::Rect\">Rect</a>&lt;T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: <a class=\"trait\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html\" title=\"trait approx::abs_diff_eq::AbsDiffEq\">AbsDiffEq</a>&lt;Epsilon = T&gt; + <a class=\"trait\" href=\"geo_types/trait.CoordNum.html\" title=\"trait geo_types::CoordNum\">CoordNum</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;T::<a class=\"type\" href=\"approx/abs_diff_eq/trait.AbsDiffEq.html#associatedtype.Epsilon\" title=\"type approx::abs_diff_eq::AbsDiffEq::Epsilon\">Epsilon</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/nightly/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,&nbsp;</span>","synthetic":false,"types":["geo_types::rect::Rect"]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()