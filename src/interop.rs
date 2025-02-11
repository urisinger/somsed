use std::collections::HashMap;

use serde::{de::Visitor, Deserialize, Deserializer, Serialize};
use serde_json::Value;

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct Graph {
    hash: String,
    #[serde(rename = "parent_hash")]
    parent_hash: Option<String>,
    thumb_url: String,
    state_url: String,
    title: String,
    access: String,
    created: String,
    state: GraphState,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct GraphState {
    pub version: u32,
    pub random_seed: String,
    pub graph: GraphMeta,
    pub expressions: Expressions,
}
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct Expressions {
    pub list: Vec<Expression>,
}
#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct GraphMeta {
    viewport: ViewportMeta,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
#[serde(rename_all = "camelCase")]
pub struct ViewportMeta {
    xmin: f64,
    ymin: f64,
    xmax: f64,
    ymax: f64,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Expression {
    Expression {
        #[serde(deserialize_with = "deserialize_id")]
        id: u32,
        latex: Option<String>,
        color: Option<Color>,
        #[serde(flatten)]
        other: HashMap<String, Value>,
    },
    #[serde(rename = "text")]
    Comment { id: String, text: String },
}
fn deserialize_id<'de, D>(deserializer: D) -> Result<u32, D::Error>
where
    D: Deserializer<'de>,
{
    deserializer.deserialize_str(StrIntVisitor {})
}
pub struct StrIntVisitor {}
impl Visitor<'_> for StrIntVisitor {
    type Value = u32;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an unsigned integer")
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        v.parse()
            .map_err(|_| E::custom(format!("failed to parse unsigned integer from {}", v)))
    }
}
#[derive(Debug, Clone, Copy)]
pub struct Color(u32);
impl<'a> Deserialize<'a> for Color {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        deserializer.deserialize_str(ColorVisitor {})
    }
}
impl Serialize for Color {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("#{:x}", self.0))
    }
}
pub struct ColorVisitor {}
impl Visitor<'_> for ColorVisitor {
    type Value = Color;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a hex color literal")?;
        Ok(())
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if !v.starts_with('#') {
            return Err(E::custom("first character of color literal not \"#\""));
        }
        u32::from_str_radix(&v[1..], 16)
            .map_err(|_| E::custom("failed to parse hex literal"))
            .map(Color)
    }
}
