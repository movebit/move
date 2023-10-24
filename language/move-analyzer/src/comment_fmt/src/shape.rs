use std::borrow::Cow;
use std::cmp::min;
use std::ops::{Add, Sub};

use crate::Config;

// width:代码的最大宽度(不包括缩进).
// indent:代码的缩进.
// offset:代码的偏移量,即在当前语句的第一行上已经输出的文本的长度.
// Shape结构体提供了一系列方法来创建和操作不同形状的代码.
// 例如,legacy方法用于创建一个遗留(legacy)形状,其中width是最后一行的最大字符数,
// indent是第一行的缩进.注意,实际上,有时候我们可能会对不是最后一行的行使用width进行限制.

// indented方法用于创建一个缩进形状,其中indent是缩进,config是配置对象.这个方法会根据配置对象的最大宽度减去缩进的宽度来确定宽度.
// with_max_width方法用于根据配置对象的最大宽度调整当前形状的宽度.
// visual_indent方法用于创建一个可视化缩进的形状,其中extra_width是额外的宽度.这个方法会根据额外的宽度调整缩进和偏移量.
// block_indent方法用于创建一个块缩进的形状,其中extra_width是额外的宽度.如果当前形状的缩进对齐为0,
// 则会创建一个新的缩进;否则,将调整缩进和偏移量.
// block_left方法用于创建一个左对齐的块形状,其中width是左对齐的宽度.它返回一个新的形状,宽度减去左对齐的宽度.
// add_offset方法用于在当前形状的偏移量上增加额外的宽度.
// block方法用于创建一个块形状,它只包含缩进.
// sub_width方法用于减小当前形状的宽度,其中width是要减去的宽度.
// offset_left方法用于在当前形状的偏移量上增加额外的宽度,并减小宽度.
// used_width方法用于计算当前形状已使用的宽度,包括缩进和偏移量.
// rhs_overhead方法用于计算右侧表达式的额外宽度,其中config是配置对象.
// comment方法用于创建一个注释形状,其中宽度是最大宽度和注释宽度的差值.
// to_string_with_newline方法用于将当前形状转换为带有换行符的字符串,其中config是配置对象.
// infinite_width方法用于创建一个拥有无限宽度的形状.
// 这些方法允许对代码的形状进行各种操作和调整,以便在代码重写和格式化过程中使用.

#[derive(Copy, Clone, Debug)]
pub struct Indent {
    // Width of the block indent, in characters. Must be a multiple of
    // Config::tab_spaces.
    pub block_indent: usize,
    // Alignment in characters.
    pub alignment: usize,
}

// INDENT_BUFFER.len() = 81
const INDENT_BUFFER_LEN: usize = 80;
const INDENT_BUFFER: &str =
    "\n                                                                                ";

impl Indent {
    pub fn new(block_indent: usize, alignment: usize) -> Indent {
        Indent {
            block_indent,
            alignment,
        }
    }

    pub fn from_width(config: &Config, width: usize) -> Indent {
        if config.hard_tabs() {
            let tab_num = width / config.tab_spaces();
            let alignment = width % config.tab_spaces();
            Indent::new(config.tab_spaces() * tab_num, alignment)
        } else {
            Indent::new(width, 0)
        }
    }

    pub fn empty() -> Indent {
        Indent::new(0, 0)
    }

    pub fn block_only(&self) -> Indent {
        Indent {
            block_indent: self.block_indent,
            alignment: 0,
        }
    }

    pub fn block_indent(mut self, config: &Config) -> Indent {
        self.block_indent += config.tab_spaces();
        self
    }

    pub fn block_unindent(mut self, config: &Config) -> Indent {
        if self.block_indent < config.tab_spaces() {
            Indent::new(self.block_indent, 0)
        } else {
            self.block_indent -= config.tab_spaces();
            self
        }
    }

    pub fn width(&self) -> usize {
        self.block_indent + self.alignment
    }

    pub fn to_string(&self, config: &Config) -> Cow<'static, str> {
        self.to_string_inner(config, 1)
    }

    pub fn to_string_with_newline(&self, config: &Config) -> Cow<'static, str> {
        self.to_string_inner(config, 0)
    }

    fn to_string_inner(&self, config: &Config, offset: usize) -> Cow<'static, str> {
        let (num_tabs, num_spaces) = if config.hard_tabs() {
            (self.block_indent / config.tab_spaces(), self.alignment)
        } else {
            (0, self.width())
        };
        let num_chars = num_tabs + num_spaces;
        if num_tabs == 0 && num_chars + offset <= INDENT_BUFFER_LEN {
            Cow::from(&INDENT_BUFFER[offset..=num_chars])
        } else {
            let mut indent = String::with_capacity(num_chars + if offset == 0 { 1 } else { 0 });
            if offset == 0 {
                indent.push('\n');
            }
            for _ in 0..num_tabs {
                indent.push('\t')
            }
            for _ in 0..num_spaces {
                indent.push(' ')
            }
            Cow::from(indent)
        }
    }
}

impl Add for Indent {
    type Output = Indent;

    fn add(self, rhs: Indent) -> Indent {
        Indent {
            block_indent: self.block_indent + rhs.block_indent,
            alignment: self.alignment + rhs.alignment,
        }
    }
}

impl Sub for Indent {
    type Output = Indent;

    fn sub(self, rhs: Indent) -> Indent {
        Indent::new(
            self.block_indent - rhs.block_indent,
            self.alignment - rhs.alignment,
        )
    }
}

impl Add<usize> for Indent {
    type Output = Indent;

    fn add(self, rhs: usize) -> Indent {
        Indent::new(self.block_indent, self.alignment + rhs)
    }
}

impl Sub<usize> for Indent {
    type Output = Indent;

    fn sub(self, rhs: usize) -> Indent {
        Indent::new(self.block_indent, self.alignment - rhs)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Shape {
    pub width: usize,
    // The current indentation of code.
    pub indent: Indent,
    // Indentation + any already emitted text on the first line of the current
    // statement.
    pub offset: usize,
}

impl Shape {
    /// `indent` is the indentation of the first line. The next lines
    /// should begin with at least `indent` spaces (except backwards
    /// indentation). The first line should not begin with indentation.
    /// `width` is the maximum number of characters on the last line
    /// (excluding `indent`). The width of other lines is not limited by
    /// `width`.
    /// Note that in reality, we sometimes use width for lines other than the
    /// last (i.e., we are conservative).
    // .......*-------*
    //        |       |
    //        |     *-*
    //        *-----|
    // |<------------>|  max width
    // |<---->|          indent
    //        |<--->|    width
    pub fn legacy(width: usize, indent: Indent) -> Shape {
        Shape {
            width,
            indent,
            offset: indent.alignment,
        }
    }

    pub fn indented(indent: Indent, config: &Config) -> Shape {
        Shape {
            width: config.max_width().saturating_sub(indent.width()),
            indent,
            offset: indent.alignment,
        }
    }

    pub fn visual_indent(&self, extra_width: usize) -> Shape {
        let alignment = self.offset + extra_width;
        Shape {
            width: self.width,
            indent: Indent::new(self.indent.block_indent, alignment),
            offset: alignment,
        }
    }

    pub fn block_indent(&self, extra_width: usize) -> Shape {
        if self.indent.alignment == 0 {
            Shape {
                width: self.width,
                indent: Indent::new(self.indent.block_indent + extra_width, 0),
                offset: 0,
            }
        } else {
            Shape {
                width: self.width,
                indent: self.indent + extra_width,
                offset: self.indent.alignment + extra_width,
            }
        }
    }

    pub fn add_offset(&self, extra_width: usize) -> Shape {
        Shape {
            offset: self.offset + extra_width,
            ..*self
        }
    }

    pub fn block(&self) -> Shape {
        Shape {
            indent: self.indent.block_only(),
            ..*self
        }
    }

    pub fn sub_width(&self, width: usize) -> Option<Shape> {
        Some(Shape {
            width: self.width.checked_sub(width)?,
            ..*self
        })
    }

    pub fn used_width(&self) -> usize {
        self.indent.block_indent + self.offset
    }

    pub fn comment(&self, config: &Config) -> Shape {
        let width = min(
            self.width,
            config.comment_width().saturating_sub(self.indent.width()),
        );
        Shape { width, ..*self }
    }

    pub fn to_string_with_newline(&self, config: &Config) -> Cow<'static, str> {
        let mut offset_indent = self.indent;
        offset_indent.alignment = self.offset;
        offset_indent.to_string_inner(config, 0)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn indent_add_sub() {
        let indent = Indent::new(4, 8) + Indent::new(8, 12);
        assert_eq!(12, indent.block_indent);
        assert_eq!(20, indent.alignment);

        let indent = indent - Indent::new(4, 4);
        assert_eq!(8, indent.block_indent);
        assert_eq!(16, indent.alignment);
    }

    #[test]
    fn indent_add_sub_alignment() {
        let indent = Indent::new(4, 8) + 4;
        assert_eq!(4, indent.block_indent);
        assert_eq!(12, indent.alignment);

        let indent = indent - 4;
        assert_eq!(4, indent.block_indent);
        assert_eq!(8, indent.alignment);
    }

    #[test]
    fn indent_to_string_spaces() {
        let config = Config::default();
        let indent = Indent::new(4, 8);

        // 12 spaces
        assert_eq!("            ", indent.to_string(&config));
    }

    #[test]
    fn indent_to_string_hard_tabs() {
        let mut config = Config::default();
        config.set().hard_tabs(true);
        let indent = Indent::new(8, 4);

        // 2 tabs + 4 spaces
        assert_eq!("\t\t    ", indent.to_string(&config));
    }

    #[test]
    fn shape_visual_indent() {
        let config = Config::default();
        let indent = Indent::new(4, 8);
        let shape = Shape::legacy(config.max_width(), indent);
        let shape = shape.visual_indent(20);

        assert_eq!(config.max_width(), shape.width);
        assert_eq!(4, shape.indent.block_indent);
        assert_eq!(28, shape.indent.alignment);
        assert_eq!(28, shape.offset);
    }

    #[test]
    fn shape_block_indent_without_alignment() {
        let config = Config::default();
        let indent = Indent::new(4, 0);
        let shape = Shape::legacy(config.max_width(), indent);
        let shape = shape.block_indent(20);

        assert_eq!(config.max_width(), shape.width);
        assert_eq!(24, shape.indent.block_indent);
        assert_eq!(0, shape.indent.alignment);
        assert_eq!(0, shape.offset);
    }

    #[test]
    fn shape_block_indent_with_alignment() {
        let config = Config::default();
        let indent = Indent::new(4, 8);
        let shape = Shape::legacy(config.max_width(), indent);
        let shape = shape.block_indent(20);

        assert_eq!(config.max_width(), shape.width);
        assert_eq!(4, shape.indent.block_indent);
        assert_eq!(28, shape.indent.alignment);
        assert_eq!(28, shape.offset);
    }
}
